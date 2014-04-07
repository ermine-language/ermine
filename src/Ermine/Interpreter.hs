{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

--------------------------------------------------------------------
-- |
-- Copyright :  (c) Dan Doel 2014
-- License   :  BSD3
-- Maintainer:  Dan Doel <dan.doel@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------

module Ermine.Interpreter
  ( Address
  , Closure
  , code
  , frame
  , Value(..)
  , GEnv
  , LEnv
  , Ref(..)
  , Tag
  , LambdaForm(LambdaForm)
  , Func(..)
  , freeArity
  , boundArity
  , update
  , body
  , standardConstructor
  , assembleBinding
  , assemble
  , Code(..)
  , Frame(..)
  , MachineState
  , args
  , stack
  , genv
  , eval
  ) where

import Bound
import Bound.Var
import Control.Applicative hiding (empty)
import Control.Monad.Primitive
import Control.Monad.State
import Control.Lens
import Data.Default
import Data.Foldable (toList)
import Data.List (elemIndex, nub)
import Data.Map hiding (null, update, toList, filter)
import Data.Maybe
import Data.Primitive.MutVar
import Data.Traversable
import Data.Word
import Ermine.Syntax.Core (Core, HardCore, Match(..))
import qualified Ermine.Syntax.Core as Core
import Prelude hiding (lookup)

------------------------------------------------------------------------------
-- Syntax
------------------------------------------------------------------------------

data Ref = Global Word32 | Local Word32
  deriving Show

makePrisms ''Ref

data Code
  = Case Code Continuation
  | App Func [Ref]
  | Let [PreClosure] Code
  | LetRec [PreClosure] Code
  | Lit Word64
  deriving Show

_Ref :: Prism' Code Ref
_Ref = prism (\r -> App (Ref r) []) $ \case
  App (Ref r) [] -> Right r
  co             -> Left co

type PreClosure = ([Ref], LambdaForm)

type Tag = Word8

data Continuation = Cont (Map Tag Code) (Maybe Code)
  deriving Show

data Func = Ref Ref | Con Tag
  deriving Show

data LambdaForm = LambdaForm
  { _freeArity :: Word32
  , _boundArity :: Word8
  , _update :: Bool
  , _body :: Code
  } deriving Show

noUpdate :: Word32 -> Word8 -> Code -> LambdaForm
noUpdate f b e = LambdaForm f b False e

doUpdate :: Word32 -> Code -> LambdaForm
doUpdate f e = LambdaForm f 0 True e

standardConstructor :: Word32 -> Tag -> LambdaForm
standardConstructor w t = LambdaForm w 0 False . App (Con t) $ Local <$> [0..w-1]

genericLength :: Num n => [a] -> n
genericLength = fromIntegral . length

genericElemIndex :: (Eq a, Num n) => a -> [a] -> Maybe n
genericElemIndex x = fmap fromIntegral . elemIndex x

assembleBinding :: Eq v => (v -> Ref) -> Core v -> PreClosure
assembleBinding cxt co = (,) vs $ case co of
  -- Assumes that core lambdas are never 0-arity
  Core.Lam ccvs body ->
    noUpdate fvn bvn (assemble (fvn + fromIntegral bvn) cxt'' $ fromScope body)
   where bvn  = genericLength ccvs
         cxt'' (F v) = cxt' v
         cxt'' (B b) = Local $ fvn + fromIntegral b
  -- TODO: this is naive
  _ -> doUpdate fvn (assemble fvn cxt' $ co)
 where
 (fvs, vs) =
   unzip . filter (has $ _2._Local) . fmap (\v -> (v, cxt v)) . nub . toList $ co
 fvn = genericLength vs

 cxt' v | Just n <- genericElemIndex v fvs = Local n
        | otherwise                        = cxt v

assemble :: Eq v => Word32 -> (v -> Ref) -> Core v -> Code
assemble _ cxt (Core.Var v) = App (Ref $ cxt v) []
assemble _ _   (Core.HardCore hc) = assembleHardCore hc
assemble n cxt (Core.App _   f x) = assembleApp n cxt [x] f
assemble n cxt l@Core.Lam{} =
  Let [assembleBinding cxt l] (App (Ref $ Local n) [])
assemble n cxt (Core.Case e bs d) =
  (if null cs then id else Let cs) . Case e' $ assembleBranches n' ev cxt bs d
 where
 (cs, n', ev, e') = case e of
   Core.Var v -> ([], n, cxt v, _Ref # cxt v)
   _          -> ([assembleBinding cxt e], n+1, Local n, review _Ref . Local $ n)
assemble n cxt (Core.Let bs e) =
  Let bs' . assemble (n + genericLength bs) cxt' $ fromScope e
 where
 cxt' (F v) = cxt v
 cxt' (B b) = Local $ n + b
 bs' = assembleBinding cxt' . fromScope <$> bs
assemble _ _   (Core.Data _    _ _ _ ) = error "assemble: Data"
assemble _ _   (Core.Dict _ _) = error "assemble: Dict"
assemble _ _   (Core.CaseLit _ _ _ _) = error "assemble: CaseLit"

-- TODO: handle calling conventions
assembleBranches :: Eq v
                 => Word32
                 -> Ref
                 -> (v -> Ref)
                 -> Map Word8 (Match Core v)
                 -> Maybe (Scope () Core v)
                 -> Continuation
assembleBranches n ev cxt bs d = Cont bs' d'
 where
 bs' = bs <&> \(Match ccvs _ e) ->
   let cxt' = unvar bc cxt
       bc 0 = ev
       bc b = Local . (n-1+) . fromIntegral $ b
    in assemble (n+genericLength ccvs) cxt' (fromScope e)
 d' = assemble n (unvar (const ev) cxt) . fromScope <$> d

anf :: Eq v
    => Word32 -> (v -> Ref)
    -> LensLike (State (Word32, [PreClosure])) s t (Core v) Ref
    -> s -> (t, (Word32, [PreClosure]))
anf n cxt ll s = runState (ll assemblePiece s) (n, [])
 where
 assemblePiece (Core.Var v) = return $ cxt v
 assemblePiece co = state $ \(k,l) ->
   let bnd = assembleBinding cxt co
    in (Local k,(k+1,bnd:l))

assembleApp :: Eq v => Word32 -> (v -> Ref) -> [Core v] -> Core v -> Code
assembleApp n cxt xs (Core.App _ f x) = assembleApp n cxt (x:xs) f
assembleApp n cxt xs f = (if null bs then id else Let bs) $ App (Ref f') xs'
 where
 ((f', xs'), (_, bs)) = anf n cxt (beside id traverse) (f, xs)
assembleHardCore :: HardCore -> Code
assembleHardCore _ = undefined

------------------------------------------------------------------------------
-- Evaluation
------------------------------------------------------------------------------

newtype Address m = Address (MutVar (PrimState m) (Closure m))
  deriving Eq

data Value m = Addr (Address m) | Prim Word64

data Closure m = Closure
  { _code :: LambdaForm
  , _frame :: [Value m]
  }

type LEnv m = Map Word32 (Value m)

type GEnv m = Map Word32 (Address m)

data Frame m
  = Branch Continuation (LEnv m)
  | Update (Address m) [Value m]

data MachineState m = MachineState
  { _args :: [Value m]
  , _stack :: [Frame m]
  , _genv :: GEnv m
  }

allocClosure :: (Functor m, PrimMonad m)
             => GEnv m -> LEnv m -> PreClosure -> m (Address m)
allocClosure gl lo cc = Address <$> (newMutVar $ buildClosure gl lo cc)

allocRecursive :: (Applicative m, PrimMonad m)
               => GEnv m -> LEnv m -> [PreClosure] -> m (LEnv m)
allocRecursive gl lo ccs = do
  addrs <- for ccs $ \_ -> Address <$> newMutVar undefined
  let lo' = extend lo $ Addr <$> addrs
  (lo<$) . sequence_ $ zipWith (flip poke . buildClosure gl lo') ccs addrs

buildClosure :: GEnv m -> LEnv m -> PreClosure -> Closure m
buildClosure gl lo (captures, code) = Closure code (resolve gl lo <$> captures)

poke :: PrimMonad m => Address m -> Closure m -> m ()
poke (Address r) c = writeMutVar r c

peek :: PrimMonad m => Address m -> m (Closure m)
peek (Address r) = readMutVar r

extend :: Integral k => Map k a -> [a] -> Map k a
extend lo vs = fromList ([w ..] `zip` vs) `union` lo
 where w = fresh lo

resolve :: GEnv m -> LEnv m -> Ref -> Value m
resolve gl _  (Global gw) = case lookup gw gl of
  Nothing -> error $ "PANIC: bad global name reference: " ++ show gw
  Just ad -> Addr ad
resolve _  lo (Local  lw) = case lookup lw lo of
  Nothing -> error $ "PANIC: bad local name reference: " ++ show lw
  Just va -> va

instance Default (MachineState m) where
  def = MachineState [] [] empty

fresh :: Integral k => Map k a -> k
fresh m = case maxViewWithKey m of Nothing -> 0 ; Just ((w,_),_) -> w+1

makeLenses ''Closure
makeLenses ''LambdaForm
makeLenses ''MachineState

pushArgs :: [Value m] -> MachineState m -> MachineState m
pushArgs as = over args (as ++)

pushUpdate :: Address m -> MachineState m -> MachineState m
pushUpdate addr ms = push (Update addr $ ms^.args) ms & args .~ []

push :: Frame m -> MachineState m -> MachineState m
push fr = stack %~ (fr:)

pop :: MachineState m -> Maybe (Frame m, MachineState m)
pop ms = case ms^.stack of f:fs -> Just (f, ms & stack .~ fs) ; [] -> Nothing

select :: Continuation -> Tag -> Code
select (Cont bs df) t =
  fromMaybe (error "PANIC: missing default case in branch") $ lookup t bs <|> df

eval :: (Applicative m, PrimMonad m)
     => MachineState m -> Code -> LEnv m -> m (Either (Closure m) Word64)
eval ms (App f xs0) lo = case f of
  Ref r -> case resolve gl lo r of
    Addr a -> enter (pushArgs xs ms) a
    Prim w | null xs0  -> returnLit ms w
           | otherwise -> error "PANIC: primitive applied to arguments"
  Con t -> returnCon ms t xs
 where
 gl = ms^.genv
 xs = resolve gl lo <$> xs0
eval ms (Let bs e) lo = do
  cs <- traverse (allocClosure (ms^.genv) lo) bs
  let w = fresh lo
  eval ms e (fromList (zip [w ..] . fmap Addr $ cs) `union` lo)
eval ms (LetRec bs e) lo = do
  lo' <- allocRecursive (ms^.genv) lo bs
  eval ms e lo'
eval ms (Case co k) lo = eval (push (Branch k lo) ms) co lo
eval ms (Lit l) _ = returnLit ms l

enter :: (Applicative m, PrimMonad m)
      => MachineState m -> Address m -> m (Either (Closure m) Word64)
enter ms addr = peek addr >>= \case
  cl@(Closure co da)
    | co^.update -> eval (pushUpdate addr ms) (co^.body) (fromList $ zip [0..] da)
    | ar <= length argz -> eval (ms & args .~ rest) (co^.body) lenv
    | otherwise -> case pop ms of
      Just (Update ad st, ms') -> do
        let nf = co^.freeArity + genericLength argz
            nb = co^.boundArity - genericLength argz
        poke ad $ Closure (noUpdate nf nb $ co^.body) (Addr addr : argz)
        enter (ms' & args %~ (++st)) addr
      Just (Branch _ _,_) -> error "under-applied function in branch"
      Nothing -> return $ Left cl
   where
   argz = ms^.args
   ar = fromIntegral $ co^.boundArity
   (first, rest) = splitAt ar argz
   lenv = fromList $ zip [0..] (da ++ first)

returnCon :: (Applicative m, PrimMonad m)
          => MachineState m -> Tag -> [Value m] -> m (Either (Closure m) Word64)
returnCon ms t vs = case pop ms of
  Just (Branch k lo, ms') -> eval ms' (select k t) (extend lo vs)
  Just (Update ad st, ms')
    | null $ ms'^.args -> do
      poke ad (Closure (standardConstructor (genericLength vs) t) vs)
      returnCon (ms' & args .~ st) t vs
    | otherwise -> error "PANIC: update with non-empty arg stack"
  Nothing -> return . Left $
    Closure (standardConstructor (genericLength vs) t) vs

returnLit :: PrimMonad m => MachineState m -> Word64 -> m (Either (Closure m) Word64)
returnLit _ _ = error "returnLit: unimplemented"
