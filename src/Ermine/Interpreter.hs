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
  , Var(..)
  , Tag
  , LambdaForm(LambdaForm)
  , Func(..)
  , freeArity
  , boundArity
  , update
  , body
  , standardConstructor
  , Code(..)
  , Frame(..)
  , MachineState
  , args
  , stack
  , genv
  , eval
  ) where

import Control.Applicative hiding (empty)
import Control.Monad.Primitive
import Control.Lens
import Data.Default
import Data.Map hiding (null, update)
import Data.Maybe
import Data.Primitive.MutVar
import Data.Traversable
import Data.Word
import Prelude hiding (lookup)

------------------------------------------------------------------------------
-- Syntax
------------------------------------------------------------------------------

data Var = Global Word32 | Local Word32
  deriving Show

data Code
  = Case Code Continuation
  | App Func [Var]
  | Let [([Var], LambdaForm)] Code
  | LetRec [([Var], LambdaForm)] Code
  | Lit Word64
  deriving Show

type Tag = Word8

data Continuation = Cont (Map Tag Code) (Maybe Code)
  deriving Show

data Func = Var Var | Con Tag
  deriving Show

data LambdaForm = LambdaForm
  { _freeArity :: Word32
  , _boundArity :: Word8
  , _update :: Bool
  , _body :: Code
  } deriving Show

standardConstructor :: Word32 -> Tag -> LambdaForm
standardConstructor w t = LambdaForm w 0 False . App (Con t) $ Local <$> [0..w-1]

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
             => GEnv m -> LEnv m -> ([Var], LambdaForm) -> m (Address m)
allocClosure gl lo cc = Address <$> (newMutVar $ buildClosure gl lo cc)

allocRecursive :: (Applicative m, PrimMonad m)
               => GEnv m -> LEnv m -> [([Var], LambdaForm)] -> m (LEnv m)
allocRecursive gl lo ccs = do
  addrs <- for ccs $ \_ -> Address <$> newMutVar undefined
  let lo' = extend lo $ Addr <$> addrs
  (lo<$) . sequence_ $ zipWith (flip poke . buildClosure gl lo') ccs addrs

buildClosure :: GEnv m -> LEnv m -> ([Var], LambdaForm) -> Closure m
buildClosure gl lo (captures, code) = Closure code (resolve gl lo <$> captures)

poke :: PrimMonad m => Address m -> Closure m -> m ()
poke (Address r) c = writeMutVar r c

peek :: PrimMonad m => Address m -> m (Closure m)
peek (Address r) = readMutVar r

extend :: Integral k => Map k a -> [a] -> Map k a
extend lo vs = fromList ([w ..] `zip` vs) `union` lo
 where w = fresh lo

resolve :: GEnv m -> LEnv m -> Var -> Value m
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
  Var v -> case resolve gl lo v of
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
        let nf = co^.freeArity + fromIntegral (length argz)
            nb = co^.boundArity - fromIntegral (length argz)
        poke ad $ Closure (LambdaForm nf nb False $ co^.body) (Addr addr : argz)
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
      poke ad (Closure (standardConstructor (fromIntegral $ length vs) t) vs)
      returnCon (ms' & args .~ st) t vs
    | otherwise -> error "PANIC: update with non-empty arg stack"
  Nothing -> return . Left $
    Closure (standardConstructor (fromIntegral $ length vs) t) vs

returnLit :: PrimMonad m => MachineState m -> Word64 -> m (Either (Closure m) Word64)
returnLit _ _ = error "returnLit: unimplemented"
