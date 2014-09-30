module Foreign.Iteratee.Input where

import Function

foreign data "scalaz.Input" Input a

empty : Input i
empty = fnEmpty emptyModule

eof : Input i
eof = fnEOF eofModule

-- el : i -> Input i
-- el i = fnEl elModule i -- (delay i)

-- input : Input a -> b -> (a -> b) -> b -> b
-- input i x f y = foldInput# i (delay x) (f . force) (delay y)

private foreign
  data "scalaz.IterV$Empty$" Empty
  method "apply" fnEmpty : forall i. Empty -> Input i
  value "scalaz.IterV$El$" "MODULE$" emptyModule : Empty
  data "scalaz.IterV$EOF$" EOF
  value "scalaz.IterV$El$" "MODULE$" eofModule : EOF
  method "apply" fnEOF : forall i. EOF -> Input i
  data "scalaz.IterV$El$" El
  value "scalaz.IterV$El$" "MODULE$" elModule : El
--  method "apply" fnEl : forall i. El -> Function0 i -> Input i
--  method "apply" foldInput# : forall a b. Input a -> Function0 b -> (Function0 a -> b) -> Function0 b -> b
