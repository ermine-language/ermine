module Native.NonEmpty where

import Function
import Native.List

foreign
  data "scalaz.NonEmptyList" NonEmpty# (a: *)

listNel# : NonEmpty# a -> List a
listNel# = fromList# . listNel##

private foreign
  method "list" listNel## : NonEmpty# a -> List# a
