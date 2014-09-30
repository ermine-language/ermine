module Relation.Aggregate where

-- `aggregate` yields a computed single-row, single-column relation
-- with a value computed from the underlying relations and a
-- particular aggregation operator like `count`.  We define
-- `aggregate` and the aggregation operators here.

import Relation.Aggregate.Unsafe
import Function
import Native.Function
import Relation.Op using asOp
import Relation.Op.Type
import Relation.Aggregate.Unsafe
import Field
export Relation.Aggregate.Type

countAgg : PrimitiveNum n => Aggregate (| |) n
countAgg = countModule

sum, mean, stddev, variance : (PrimitiveNum n, AsOp op) => op r n -> Aggregate r n
sum = funcall1# sumModule . asOp
mean = funcall1# avgModule . asOp
avg = mean
stddev = funcall1# stddevModule . asOp
variance = funcall1# varianceModule . asOp
min, max : (Primitive n, AsOp op) => op r n -> Aggregate r n
min = funcall1# minModule . asOp
max = funcall1# maxModule . asOp

count r = aggregate countAgg Count r
sumBy f r = aggregateBy sum f r
meanBy f r = aggregateBy avg f r
minBy f r = aggregateBy min f r
maxBy f r = aggregateBy max f r
standardDeviationBy f r = aggregateBy stddev f r
varianceBy f r = aggregateBy variance f r

aggregateBy : (Relational rel, r <- (h,t))
           => (Field h n -> Aggregate h n)
           -> Field h n
           -> rel r
           -> rel h
aggregateBy a f r = aggregate (a f) f r

-- builtin
--   aggregate : forall r a f s.
--               (exists u. s <- (r, u)) =>
--               Aggregate r a -> Field f a -> [..s] -> [..f]
