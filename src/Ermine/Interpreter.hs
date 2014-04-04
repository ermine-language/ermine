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
  ) where

import Control.Applicative hiding (empty)
import Control.Monad.State
import Control.Lens
import Data.List (genericLength)
import Data.Map
import Data.Maybe
import Data.Word
import Ermine.Syntax.Literal as Literal
import Prelude hiding (lookup)

-- newtype Address s = Address (STRef s Closure)
newtype Address s = Address Word64 deriving (Eq, Ord, Show, Read)

data Closure s = Closure { _code :: LambdaForm
                         , _data :: [Value s]
                         }

data Value s = Addr (Address s) | Prim Word64

type Env s = Map Word32 (Address s)

type Heap s = Map (Address s) (Closure s)

data Var = Global Word32 | Local Word32

type Tag = Word8

data LambdaForm = LForm
                { _freeArity :: Word32
                , _boundArity :: Word8
                , _update :: Bool
                , _body :: Code
                }

data Code
  = Case Code Continuation
  | App Func [Var]
  | Let [([Var], LambdaForm)] Code
  | LetRec [([Var], LambdaForm)] Code
  | Lit Literal

data Continuation = Cont (Map Tag Code) (Maybe Code)

data Func = Var Var | Con Tag

data Update

data Frame s = Branch Continuation (Env s)
             | Update (Address s)

data MachineState s
  = ST { _args :: [Value s]
       , _stack :: [Frame s]
       , _genv :: Env s
       }

newtype Exec s a = Exec { _exec :: State (Heap s) a }
  deriving (Functor, Applicative, Monad, MonadState (Heap s))

exec :: (forall s. Exec s a) -> a
exec m = evalState (_exec m) empty

allocClosure :: Env s -> Env s -> ([Var], LambdaForm) -> Exec s (Address s)
allocClosure gl lo cc = state $ \heap -> case findMax heap of
  (Address w, _) -> (Address $ w+1, insert (Address $ w+1) cl heap)
 where cl = buildClosure gl lo cc

allocRecursive :: Env s -> Env s -> [([Var], LambdaForm)] -> Exec s (Env s)
allocRecursive gl lo ccs = state $ \heap ->
  case (fst $ findMax heap, fst $ findMax lo) of
    (Address w0, n0) -> (lo', alloced `union` heap)
     where
     w = w0+1
     n = n0+1
     addrs = zipWith (const . Address) [w ..] ccs
     lo' = fromList (zip [n..] addrs) `union` lo
     alloced = fromList . zip addrs $ buildClosure gl lo' <$> ccs

buildClosure :: Env s -> Env s -> ([Var], LambdaForm) -> Closure s
buildClosure gl lo (captures, code) = Closure code (resolve gl lo <$> captures)

follow :: Address s -> Exec s (Closure s)
follow addr = state $ \heap -> (heap ! addr, heap)

resolve :: Env s -> Env s -> Var -> Value s
resolve gl _  (Global gw) = Addr $ gl ! gw
resolve _  lo (Local  lw) = Addr $ lo ! lw

makeLenses ''LambdaForm
makeLenses ''MachineState

pushArgs :: [Value s] -> MachineState s -> MachineState s
pushArgs as = over args (as ++)

push :: Frame s -> MachineState s -> MachineState s
push frame = stack %~ (frame:)

select :: Continuation -> Tag -> Code
select (Cont bs df) t =
  fromMaybe (error "PANIC: missing default case in branch") $ lookup t bs <|> df

eval :: MachineState s -> Code -> Env s -> Exec s ()
eval ms (App f xs0) lo = case f of
  Var v -> case resolve gl lo v of
    Addr a -> enter (pushArgs xs ms) a
    Prim _ -> error "PANIC: primitive applied to arguments"
  Con t -> returnCon ms t xs
 where
 gl = ms^.genv
 xs = resolve gl lo <$> xs0
eval ms (Let bs e) lo = do
  cs <- traverse (allocClosure (ms^.genv) lo) bs
  let (w, _) = findMax lo
  eval ms e (fromDistinctAscList (zip [w+1 ..] cs) `union` lo)
eval ms (LetRec bs e) lo = do
  lo' <- allocRecursive (ms^.genv) lo bs
  eval ms e lo'
eval ms (Case code k) lo = eval (push (Branch k lo) ms) code lo
eval ms (Lit l) _ = returnLit ms l

enter :: MachineState s -> Address s -> Exec s ()
enter ms addr = follow addr >>= \case
  Closure co _
    | co^.boundArity <= genericLength (ms^.args) -> error "enter: unimplemented"
    | otherwise -> error "enter: unimplemented"

returnCon :: MachineState s -> Tag -> [Value s] -> Exec s ()
returnCon _  _ _  = error "returnCon: unimplemented"

returnLit :: MachineState s -> Literal -> Exec s ()
returnLit _ _ = error "returnLit: unimplemented"
