{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TemplateHaskell #-}

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

import Control.Applicative
import Control.Lens
import Data.List (genericLength)
import Data.Map
import Data.Maybe
import Data.Word
import Ermine.Syntax.Literal
import Prelude hiding (lookup)

-- newtype Address s = Address (STRef s Closure)
newtype Address = Address Word64 deriving (Eq, Ord, Show, Read)

data Closure = Closure { _code :: LambdaForm
                       , _data :: [Value]
                       }

data Value = Addr Address | Prim Word64

type Env = Map Word32 Address

type Heap = Map Address Closure

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

data MachineState
  = ST { _args :: [Value]
       , _rets :: [(Continuation, Env)]
       , _updates :: [Update]
       , _heap :: Heap
       , _genv :: Env
       }

makeLenses ''LambdaForm
makeLenses ''MachineState

val :: Env -> Env -> Var -> Value
val glo _   (Global gw) = Addr $ glo ! gw
val _   loc (Local  lw) = Addr $ loc ! lw

push :: [a] -> [a] -> [a]
push = (++)

select :: Continuation -> Tag -> Code
select (Cont bs df) t =
  fromMaybe (error "PANIC: missing default case in branch") $ lookup t bs <|> df

eval :: MachineState -> Code -> Env -> ()
eval ms (App f xs0) e = case f of
  Var v -> case resolve v of
    Addr a -> enter (ms & args %~ push xs) a
  Con t -> returnCon ms t xs
 where
 resolve = val e (ms^.genv)
 xs = resolve <$> xs0
eval _ _ _ = error "eval: unimplemented"

enter :: MachineState -> Address -> ()
enter ms a = case lookup a (ms^.heap) of
  Just (Closure co _ )
    | co^.boundArity <= genericLength (ms^.args) -> error "enter: unimplemented"
  Nothing -> error "PANIC: bad address"

returnCon :: MachineState -> Tag -> [Value] -> ()
returnCon _  _ _  = error "returnCon: unimplemented"
