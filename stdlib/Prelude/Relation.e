module Relation where

import Native.List
import Native.Pair
import Native.Relation
import Function
export Relation.Sort
export Relation.Aggregate as Aggregate
import Relation.Predicate
import Relation.Row
import Relation.Op
import List
import Constraint
import Field
import Syntax.List using map
import Date
import Primitive as P
import Bool as B

relation : List {..r} -> Relation (|..r|)
relation r = mkRelation# (toList# r)

mem : List {..r} -> Mem (|..r|)
mem r = mkMem# (toList# r)

row : {..r} -> Relation (|..r|)
row r = relation [r]

type Relation a = [..a]

rheader : Relational rel => rel a -> Row a
rheader = rheader# . relation#

filterEq : forall c r a . Has r c => Field c a -> a -> [..r] -> [..r]
filterEq f a r = join r (relation [{f = a}])

firstBy : Has r h => Row h -> [..r] -> [..r]
firstBy f r = join r (topK f 1 (project f r))

lastBy : Has r h => Row h -> [..r] -> [..r]
lastBy f r = join r (bottomK f 1 (project f r))

rheader# : Relation# -> Row a
rheader# = Row . map fromPair# . fromList# . header#

setColumn f op r = case existentialF "combine_tmp" (fieldType f) of
  EField ft -> rename ft f (except (single f) (combine op ft r))

materialize r = letR r id
toMem r = letM r id

-- remove any rows whose nodeid is someone else's parentid
leafRows : r <- (parent,child,t) => Field parent n -> Field child n -> [..r] -> [..r]
leafRows p i r = difference r (r # {i} ` join ' [| i <- p |] (r # {p}) ` join ' r)

joinWithDefault : forall extra a r1 r2 r3. (exists c s. r1 <- (c, s), r2 <- (c, extra), r3 <- (c, s, extra), PrimitiveAtom a)
               => Field extra a -> a
               -> Mem r1 -> Mem r2 -> Mem r3
joinWithDefault ef d min mout = case existentialF "joinWithDefault_tmp" (withNull (typeOfOp ef)) of
  EField fj -> hashLeftJoin fj min (promote ef fj mout)
            |> combine (coalesce fj (prim d)) ef
            |> except {fj}


-- lookup based on the columns that intersect, then replace base with extra
-- and base may or may not be in intersection
-- partialLookup : (s | extra, key <- (base, t), r1 <- (key, s), r2 <- (key, extra), PrimitiveAtom a)
--             => Field base a -> Field extra a
--             -> Mem r2 -> Mem r1 -> Mem r1
partialLookup : (PrimitiveAtom a, kv <- (key,val), r <- (k,base))
             => Field key a
             -> Field val a
             -> Mem kv
             -> Mem r
             -> Mem r
partialLookup bf ef mout min =
  partialLookup' bf ef mout min
  |> except {bf}
  |> rename ef bf

partialLookup' : (PrimitiveAtom a, kv <- (key,val), r <- (k,base), r2 <- (k,v,base))
             => Field key a
             -> Field val a
             -> Mem kv
             -> Mem r
             -> Mem r2
partialLookup' bf ef mout min = case existentialF "partialLookup_tmp" (withNull (typeOfOp bf)) of
  EField fj -> hashLeftJoin fj min (promote ef fj mout)
            |> combine (coalesce fj bf) ef
            |> except {fj}

groupBy : (Relational rel, kv <- (k,v), kv2 <- (k,v2))
       => Row k -> (Mem v -> Mem v2) -> rel kv -> Mem kv2
groupBy (Row k) f r = groupBy# (toList# (map toPair# k)) f (asMem r)

count = count_Aggregate
sumBy = sumBy_Aggregate
meanBy = meanBy_Aggregate
minBy_A = minBy_Aggregate
maxBy = maxBy_Aggregate
standardDeviationBy = standardDeviationBy_Aggregate
varianceBy = varianceBy_Aggregate

maxRowBy f r = join (maxBy f r) r
minRowBy f r = join (minBy_A f r) r

unionAll : List (Relation r) -> Relation r
unionAll (h::t) = foldl union h t
unionAll [] = relation []

-- | Removes `f2`, then renames `f1` to `f2`
rename' f1 f2 = rename f1 f2 . except { f2 }

-- | Identical behavior to `join`, but requires a witness that
-- the intersection is nonempty, to guard against accidental
-- cartesian joins
join1 : (ra <- (k, r1), rb <- (k,r2), r <- (k,r1,r2), Relational rel)
     => Field k a
     -> rel ra
     -> rel rb
     -> rel r
join1 f r r2 = join r r2

-- | Identical behavior to `join`, but specifies the join key with a
-- row witness, generating a type error if the common column set does
-- not match this row witness.
joinBy : (r1 <- (k,t1), r2 <- (k,t2), r <- (k,t1,t2), Relational rel)
      => Row k -> rel r1 -> rel r2 -> rel r
joinBy r = join

-- | Like `joinBy`, but specifies only that the join key is a superset
-- the columns specified by the given row witness.
joinBy' : (r1 <- (kh,kt,t1), r2 <- (kh,kt,t2), r <- (kh,kt,t1,t2), Relational rel)
       => Row kh -> rel r1 -> rel r2 -> rel r
joinBy' r = join

-- | Get rows whose date is equal to the given date, looking backward
-- arbitrarily far if needed. Also see `lookupLatestWithin`.
lookupLatest1 : r <- (h,t) => Field h Date -> Date -> Relation r -> Relation r
lookupLatest1 f d r = lookupLatest f (row { f = d }) r

lookupLatest : r <- (h,t) => Field h Date -> Relation h -> Relation r -> Relation r
lookupLatest f ds r = withFieldCopy f (f' ->
  nearestDate f (r # {f}) f' (rename f f' ds)
  |> rename f' f
  |> join r)

lookupLatestWithin : r <- (h,t) => (Field h Date -> Op h Date)
                  -> Field h Date -> Relation h -> Relation r -> Relation r
lookupLatestWithin upperBound f ds r = withFieldCopy f (f' ->
  nearestDateWithin upperBound f (r # {f}) f' (rename f f' ds)
  |> rename f' f
  |> join r)

lookupLatestWithin1 :
  r <- (h,t) => (Field h Date -> Op h Date)
             -> Field h Date -> Date -> Relation r -> Relation r
lookupLatestWithin1 upperBound f d = lookupLatestWithin upperBound f (row { f = d })

-- | Example: `lookbackJoin (addDate 5 days) pricingDate r1 r2`
lookbackJoin b f r1 r2 = withFieldCopy f (f' ->
  let j = nearestDateWithin b f (r1 # {f}) f' (rename f f' r2 # {f'})
      r1' = except {f'} . [| f = f' |] ' join j r1
  in join r1' r2)

-- | Compute mapping from `rsparse` to `rfine`, where each
-- `rsparse` date, `d`, is mapped to the closest date >= d
nearestDate : r <- (rfine,rsparse)
           => Field rsparse Date
           -> [..rsparse]
           -> Field rfine Date
           -> [..rfine]
           -> [..r]
nearestDate fsparse rsparse ffine rfine = materialize '
  groupBy {ffine} (maxRowBy fsparse) ([| fsparse <= ffine |] (join rsparse rfine))

-- | Like `nearestDate`, but bounded up to the date given by `upperBound`.
-- Example: `nearestDateWithin (addDate 5 days)`
nearestDateWithin : r <- (rfine,rsparse)
                 => (Field rsparse Date -> Op rsparse Date)
                 -> Field rsparse Date
                 -> [..rsparse]
                 -> Field rfine Date
                 -> [..rfine]
                 -> [..r]
nearestDateWithin upperBound fsparse rsparse ffine rfine = materialize '
  groupBy {ffine} (maxRowBy fsparse) ([| fsparse <= ffine, ffine <= upperBound fsparse |] (join rsparse rfine))

-- | Calls `f`, with lookback up to `oldestAllowed` to ensure a row
-- is returned for each of `keys`.
lookupLatest' : r <- (keys,t) => (Date -> [..keys] -> [..r]) -> Date -> Date -> [..keys] -> [..r]
lookupLatest' f oldestAllowed d keys =
  let rk = rheader keys
      go d1 d2 ks = if_B (d1 >=_P d2)
        (f d2 ks)
        (letR (f d2 ks) (r -> r ` union '
          go
            d1
            (decrementDate 1 days d2)
            (difference ks (r # rk))
        ))
  in go oldestAllowed d keys

replaceColumn f r r2 = except {f} (join1 f r r2)

copyColumn : (ri <- (r1,t), ro <- (r1,r2,t), Relational rel)
          => Field r1 a -> Field r2 a
          -> rel ri
          -> rel ro
copyColumn f1 f2 r = [| f2 = f1 |] r

-- builtin
--   header# : Relation# -> List# (Pair# String PrimT)
--   join : (a <- (d,e), b <- (e, f), c <- (d,e,f)) => [..a] -> [..b] -> [..c]
--   rename: (u1 <- (r1,t), u2 <- (r2,t)) => Field r1 a -> Field r2 a -> [..u1] ->: [..u2]
--   union : [..a] -> [..a] -> [..a]
--   diff : [..a] -> [..a] -> [..a]
--   data Relation#

