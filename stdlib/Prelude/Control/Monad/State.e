module Control.Monad.State where

import Control.Monad
import Pair

data State s a = State (s -> (a,s))
runState : State s a -> s -> (a, s)
runState (State f) s = f s

stateMonad : Monad (State s)
stateMonad = Monad return' bind'
private
  return' a = State (s -> (a, s))
  bind' (State x) f = State(s ->
    case (x s) of (v, s') -> runState (f v) s')

get : State s s
get = State (s -> (s, s))

put : s -> State s ()
put newState = State (_ -> ((), newState))

data StateT s m a = StateT (s -> m (a,s))
runStateT : StateT s m a -> s -> m (a, s)
runStateT (StateT f) = f

stateTMonad : Monad m -> Monad (StateT s m)
stateTMonad ad = Monad returnT' bindT' where
  returnT' a = StateT (s -> (unit ad (a, s)))
  -- bind' : StateT s m a -> (a -> StateT s m b) -> StateT s m b
  bindT' st f = StateT (s -> (bind ad (runStateT st s) (v ->
    case v of (a, s') -> runStateT (f a) s')))
