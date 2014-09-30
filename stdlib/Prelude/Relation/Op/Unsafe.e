module Relation.Op.Unsafe where

-- These symbols shouldn't be imported by Ermine programs.

import Native
import Prim using type PrimExpr#
import Relation.Op.Type
import Relation.Predicate.Type

-- the constraints here are those from Relation.join
type OpBin# v = forall a b c.
                (exists d e f. a <- (e, d), b <- (f, e), c <- (f, e, d))
                => Function2 (Op a v) (Op b v) (Op c v)

foreign
  -- Op erasure
  subtype UnsafeOp : Op r a -> Op#
  subtype PhantomOp : Op# -> Op r a
  -- Op data constructors
  value "com.clarifi.reporting.Op$OpLiteral$" "MODULE$"
      opLiteralModule : Function1 PrimExpr# (Op (| |) a)
  value "com.clarifi.reporting.Op$Add$" "MODULE$"
      addModule : OpBin# a
  value "com.clarifi.reporting.Op$Sub$" "MODULE$"
      subModule : OpBin# a
  value "com.clarifi.reporting.Op$Mul$" "MODULE$"
      mulModule : OpBin# a
  value "com.clarifi.reporting.Op$FloorDiv$" "MODULE$"
      floorDivModule : OpBin# a
  value "com.clarifi.reporting.Op$DoubleDiv$" "MODULE$"
      doubleDivModule : OpBin# a
  value "com.clarifi.reporting.Op$Pow$" "MODULE$"
      powModule : OpBin# a
  value "com.clarifi.reporting.Op$Concat$" "MODULE$"
      concatModule : Function1 (List# Op#) (Op r a)
  value "com.clarifi.reporting.Op$If$" "MODULE$"
      ifModule : Function3 (Predicate r) (Op s a) (Op t a) (Op u a)
  value "com.clarifi.reporting.Op$Coalesce$" "MODULE$"
      coalesceModule : Function2 (Op s (Nullable a)) (Op t a) (Op u a)
  value "com.clarifi.reporting.Op$Funcall$" "MODULE$"
      funcallModule : Function5 String String (List# String) (List# Op#) (Prim a) (Op r a)

  method "guessTypeUnsafe" typeOfOp# : Op r a -> Prim a

-- builtin
--   data "com.clarifi.reporting.Op" Op#
--   combine# : Field r a -> Op# -> [..s] -> [..t]
