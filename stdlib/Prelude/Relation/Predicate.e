module Relation.Predicate where

import Constraint
import Native.Function
import Native.Record
import Function
import Relation.Op.Type using type Op
import Relation.Op using asOp

export Relation.Predicate.Type

infix 4 < <= == > >= !=
infixr 3 &&
infixr 2 ||

private
  type Compare = forall a b c v.
                 (exists d e f.
                         AsOp opl, AsOp opr,
                         a <- (e, d), b <- (f, e), c <- (f, e, d))
                 => opl a v -> opr b v -> Predicate c

  type Compare# = Op r v -> Op s v -> Predicate c

  type BinPred = forall r s t.
                 RUnion2 t r s
                 => Predicate r -> Predicate s -> Predicate t
  type BinPred# = Function2 (Predicate r) (Predicate s) (Predicate t)

foreign function "com.clarifi.reporting.Predicates" "IsNull" isNull : Op r a -> Predicate r

private foreign

  data "com.clarifi.reporting.Predicate$" PredicateO
  value "com.clarifi.reporting.Predicate$" "MODULE$" predicateModule : PredicateO
  method "fromRecord" fromRecord# : forall r. PredicateO -> ScalaRecord# -> Predicate r
  data "com.clarifi.reporting.Predicate$Not$" NotO
  value "com.clarifi.reporting.Predicate$Not$" "MODULE$" notModule : NotO
  method "apply" not# : NotO -> Predicate r -> Predicate r
  data "com.clarifi.reporting.Predicate$Eq$" EqO
  value "com.clarifi.reporting.Predicate$Eq$" "MODULE$" eqModule : EqO
  method "apply" eq# : EqO -> Compare#
  data "com.clarifi.reporting.Predicate$Lt$" LtO
  value "com.clarifi.reporting.Predicate$Lt$" "MODULE$" ltModule : LtO
  method "apply" lt# : LtO -> Compare#
  data "com.clarifi.reporting.Predicate$Gt$" GtO
  value "com.clarifi.reporting.Predicate$Gt$" "MODULE$" gtModule : GtO
  method "apply" gt# : GtO -> Compare#
  value "com.clarifi.reporting.Predicate$And$" "MODULE$"
    andModule : BinPred#
  value "com.clarifi.reporting.Predicate$Or$" "MODULE$"
    orModule : BinPred#

private
  opBin : (AsOp opl, AsOp opr) =>
          (Op r a -> Op s b -> z) -> (opl r a -> opr s b -> z)
  opBin f x y = f (asOp x) (asOp y)

(==), (<), (>) : Compare
(==) = opBin $ eq# eqModule
(<) = opBin $ lt# ltModule
(>) = opBin $ gt# gtModule
(>=) op1 op2 = op1 > op2 || op1 == op2
(<=) op1 op2 = op1 < op2 || op1 == op2

(&&), (||) : BinPred
(&&) = funcall2# andModule
(||) = funcall2# orModule

(!=) a b = not (a == b)

not : Predicate r -> Predicate r
not = not# notModule

-- | Require all elements of the given record to be the same.
all : {..r} -> Predicate r
all = fromRecord# predicateModule . scalaRecord# . record#

selectNulls : (AsOp op, Relational rel, r <- (h,t)) => op h a -> rel r -> rel r
selectNulls op = filter (isNull (asOp op))

selectNotNulls : (AsOp op, Relational rel, r <- (h,t)) => op h a -> rel r -> rel r
selectNotNulls op = filter (not (isNull (asOp op)))

-- others provided by Lib

-- builtin
--   filter       : t <- (a, r) => Predicate a -> [..t] -> [..t]
