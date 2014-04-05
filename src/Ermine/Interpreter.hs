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
  ( eval
  , exec
  ) where

import Control.Applicative hiding (empty)
import Control.Monad.State
import Control.Lens
import Data.Map hiding (null, update)
import Data.Maybe
import Data.Word
import Prelude hiding (lookup)

-- newtype Address s = Address (STRef s Closure)
newtype Address s = Address Word64
  deriving (Eq, Ord, Show, Read, Num, Real, Enum, Integral)

data Closure s = Closure { _code :: LambdaForm
                         , _data :: [Value s]
                         }

data Value s = Addr (Address s) | Prim Word64

type GEnv s = Map Word32 (Address s)

type LEnv s = Map Word32 (Value s)

type Heap s = Map (Address s) (Closure s)

data Var = Global Word32 | Local Word32

type Tag = Word8

data LambdaForm = LForm
                { _freeArity :: Word32
                , _boundArity :: Word8
                , _update :: Bool
                , _body :: Code
                }

standardConstructor :: Word32 -> Tag -> LambdaForm
standardConstructor w t = LForm w 0 False . App (Con t) $ Local <$> [0..w-1]

data Code
  = Case Code Continuation
  | App Func [Var]
  | Let [([Var], LambdaForm)] Code
  | LetRec [([Var], LambdaForm)] Code
  | Lit Word64

data Continuation = Cont (Map Tag Code) (Maybe Code)

data Func = Var Var | Con Tag

data Frame s = Branch Continuation (LEnv s)
             | Update (Address s) [Value s]

data MachineState s
  = ST { _args :: [Value s]
       , _stack :: [Frame s]
       , _genv :: GEnv s
       }

newtype Exec s a = Exec { _exec :: State (Heap s) a }
  deriving (Functor, Applicative, Monad, MonadState (Heap s))

exec :: (forall s. Exec s a) -> a
exec m = evalState (_exec m) empty

allocClosure :: GEnv s -> LEnv s -> ([Var], LambdaForm) -> Exec s (Address s)
allocClosure gl lo cc = state $ \heap -> case findMax heap of
  (Address w, _) -> (Address $ w+1, insert (Address $ w+1) cl heap)
 where cl = buildClosure gl lo cc

allocRecursive :: GEnv s -> LEnv s -> [([Var], LambdaForm)] -> Exec s (LEnv s)
allocRecursive gl lo ccs = state $ \heap ->
  case (fst $ findMax heap, fst $ findMax lo) of
    (Address w0, n0) -> (lo', alloced `union` heap)
     where
     w = w0+1
     n = n0+1
     addrs = zipWith (const . Address) [w ..] ccs
     lo' = fromList (zip [n..] . fmap Addr $ addrs) `union` lo
     alloced = fromList . zip addrs $ buildClosure gl lo' <$> ccs

buildClosure :: GEnv s -> LEnv s -> ([Var], LambdaForm) -> Closure s
buildClosure gl lo (captures, code) = Closure code (resolve gl lo <$> captures)

poke :: Address s -> Closure s -> Exec s ()
poke addr c = modify $ ix addr .~ c

peek :: Address s -> Exec s (Closure s)
peek addr = state $ \heap -> (heap ! addr, heap)

resolve :: GEnv s -> LEnv s -> Var -> Value s
resolve gl _  (Global gw) = Addr $ gl ! gw
resolve _  lo (Local  lw) = lo ! lw

makeLenses ''LambdaForm
makeLenses ''MachineState

pushArgs :: [Value s] -> MachineState s -> MachineState s
pushArgs as = over args (as ++)

pushUpdate :: Address s -> MachineState s -> MachineState s
pushUpdate addr ms = push (Update addr $ ms^.args) ms & args .~ []

push :: Frame s -> MachineState s -> MachineState s
push frame = stack %~ (frame:)

pop :: MachineState s -> Maybe (Frame s, MachineState s)
pop ms = case ms^.stack of f:fs -> Just (f, ms & stack .~ fs) ; [] -> Nothing

select :: Continuation -> Tag -> Code
select (Cont bs df) t =
  fromMaybe (error "PANIC: missing default case in branch") $ lookup t bs <|> df

extend :: Integral k => Map k a -> [a] -> Map k a
extend lo vs = fromList ([w+1 ..] `zip` vs) `union` lo
 where w = fst $ findMax lo

eval :: MachineState s -> Code -> LEnv s -> Exec s ()
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
  let (w, _) = findMax lo
  eval ms e (fromList (zip [w+1 ..] . fmap Addr $ cs) `union` lo)
eval ms (LetRec bs e) lo = do
  lo' <- allocRecursive (ms^.genv) lo bs
  eval ms e lo'
eval ms (Case code k) lo = eval (push (Branch k lo) ms) code lo
eval ms (Lit l) _ = returnLit ms l

enter :: MachineState s -> Address s -> Exec s ()
enter ms addr = peek addr >>= \case
  Closure co da
    | co^.update -> eval (pushUpdate addr ms) (co^.body) (fromList $ zip [0..] da)
    | ar <= length argz -> eval (ms & args .~ rest) (co^.body) lenv
    | otherwise -> case pop ms of
      Just (Update ad st, ms') -> do
        let nf = co^.freeArity + fromIntegral (length argz)
            nb = co^.boundArity - fromIntegral (length argz)
        poke ad $ Closure (LForm nf nb False $ co^.body) (Addr addr : argz)
        enter (ms' & args %~ (++st)) addr
      Just (Branch _ _,_) -> error "under-applied function in branch"
      Nothing -> return ()
   where
   argz = ms^.args
   ar = fromIntegral $ co^.boundArity
   (first, rest) = splitAt ar argz
   lenv = fromList $ zip [0..] (da ++ first)

returnCon :: MachineState s -> Tag -> [Value s] -> Exec s ()
returnCon ms t vs = case pop ms of
  Just (Branch k lo, ms') -> eval ms' (select k t) (extend lo vs)
  Just (Update ad st, ms')
    | null $ ms'^.args -> do
      poke ad (Closure (standardConstructor (fromIntegral $ length vs) t) vs)
      returnCon (ms' & args .~ st) t vs
    | otherwise -> error "PANIC: update with non-empty arg stack"
  Nothing -> return ()

returnLit :: MachineState s -> Word64 -> Exec s ()
returnLit _ _ = error "returnLit: unimplemented"
