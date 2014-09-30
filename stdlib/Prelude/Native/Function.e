module Native.Function where

-- native
--   foreign data "scala.Function0" Function1 a
--   force : Function0 a -> a
--   delay : a -> Function0 a

--   foreign data "scala.Function1" Function1 a b
--   function1 : forall (a: *) (b: *). (a -> b) -> Function1 a b

--   foreign data "scala.Function2" Function2 a b c
--   function2 : forall (a: *) (b: *) (c: *). (a -> b -> c) -> Function2 a b c

--   foreign data "scala.Function3" Function3 a b c d
--   function2 : forall (a: *) (b: *) (c: *) (d: *). (a -> b -> c -> d) -> Function3 a b c d

foreign
  --data "scala.Function3" Function3 a b c d
  --data "scala.Function3" Function3 a b c d
  data "scala.Function4" Function4 a b c d e
  data "scala.Function5" Function5 a b c d e f
  data "scala.Function6" Function6 a b c d e f g
  method "apply" funcall1# : Function1 a b -> (a -> b)
  method "apply" impureFuncall1# : Function1 a b -> (a -> IO b)
  method "apply" funcall2# : Function2 a b c -> (a -> b -> c)
  method "apply" impureFuncall2# : Function2 a b c -> (a -> b -> IO c)
  method "apply" funcall3# : Function3 a b c d -> (a -> b -> c -> d)
  method "apply" impureFuncall3# : Function3 a b c d -> (a -> b -> c -> IO d)
  method "apply" funcall4# : Function4 a b c d e -> (a -> b -> c -> d -> e)
  method "apply" impureFuncall4# : Function4 a b c d e -> (a -> b -> c -> d -> IO e)
  method "apply" funcall5# : Function5 a b c d e f -> (a -> b -> c -> d -> e -> f)
  method "apply" impureFuncall5# : Function5 a b c d e f -> (a -> b -> c -> d -> e -> IO f)
  method "apply" funcall6# : Function6 a b c d e f g -> (a -> b -> c -> d -> e -> f -> g)
  method "apply" impureFuncall6# : Function6 a b c d e f g -> (a -> b -> c -> d -> e -> f -> IO g)
