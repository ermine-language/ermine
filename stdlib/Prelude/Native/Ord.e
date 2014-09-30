module Native.Ord where

import Bool
import Eq
import Function
import Native.Bool
import Native.Function
import Native.Object
import Ord
import Primitive

foreign
  data "scala.math.Ordering" OrdScala# (a: *)
  data "scalaz.Order" Ord# (a: *)
  method "toScalaOrdering" ordScala# : Ord# a -> OrdScala# a

private foreign
  data "scalaz.Ordering" Ordering#
  value "scalaz.Ordering$LT$" "MODULE$" lT# : Ordering#
  value "scalaz.Ordering$EQ$" "MODULE$" eQ# : Ordering#
  value "scalaz.Ordering$GT$" "MODULE$" gT# : Ordering#
  data "scalaz.Order$" OrderModule
  value "scalaz.Order$" "MODULE$" orderModule : OrderModule
  method "order" fromFunction# : OrderModule -> Function2 a a Ordering# -> Ord# a
  method "order" runOrd# : Ord# a -> a -> a -> Ordering#

fromOrdering# : Ordering# -> Ordering
fromOrdering# o = if (o === lT#) LT ' if (o === eQ#) EQ GT

ordering# : Ordering -> Ordering#
ordering# LT = lT#
ordering# EQ = eQ#
ordering# GT = gT#

ord# : Ord a -> Ord# a
ord# = fromFunction# orderModule . function2 . f2map ordering#

fromOrd# : Ord# a -> Ord a
fromOrd# = f2map fromOrdering# . runOrd#

private
  f2map f bf a = f . bf a       -- (.) . (.)
