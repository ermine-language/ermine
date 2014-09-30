module Foreign.Iteratee where

export Foreign.Iteratee.Input
import Function

foreign data "scalaz.IterV" IterV a b

cont : (Input i -> IterV i a) -> IterV i a
cont = fnCont contModule

-- done : a -> Input i -> IterV i a
-- done a i = fnDone doneModule (delay a) (delay i)

-- foldIterV : IterV e a -> (a -> Input e -> b) -> ((Input e -> IterV e a) -> b) -> b
-- foldIterV i k = foldIterV# i (function2 (da die -> k (force a) (force die)))

private foreign
  data "scalaz.IterV$Cont$" Cont
  method "apply" fnCont : forall a i. Cont -> (Input i -> IterV i a) -> IterV i a
  value "scalaz.IterV$Cont$" "MODULE$" contModule : Cont
  data "scalaz.IterV$Done$" Done
  value "scalaz.IterV$Done$" "MODULE$" doneModule : Done
--  method "apply" fnDone : forall a i. Done -> Function0 a -> Function0 (Input i) -> IterV i a
--  method "fold" foldIterV# : forall e a b. IterV e a -> (Function2 (Function0 a) (Function0 (Input e)) b) -> ((Input e -> IterV e a) -> b) -> b

