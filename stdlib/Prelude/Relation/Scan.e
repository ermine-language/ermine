module Relation.Scan where

import Prim
import Maybe
import Bool
import Control.Monad.Cont
import Control.Monad
import Control.Functor
import Relation.Row
import Relation.Aggregate as A
import Relation.Op using typeOfOp
import Field
import Function
import Pair
import Ord
import Eq
import Syntax.Relation

import Vector as V
import List as L

type Vector = Vector_V
map_L = fmap listFunctor_L

data Scan z a = Scan (Cont z (Vector a))

data RunScan (f: * -> *) z = RunScan (forall (r:row) . Relational g => g r -> (f {..r} -> z) -> z)

consume : (Vector a -> b) -> Scan z a -> Cont z b
consume f (Scan c) = fmap contFunctor f c

fromRelation : Relational f => RunScan List z -> f r -> Scan z {..r}
fromRelation scanner = scan (adapt scanner)

fromList : List a -> Scan z a
fromList = fromVector . vector_V

fromVector : Vector a -> Scan z a
fromVector = Scan . pure contAp

transform : (Vector v -> Vector v2) -> Scan z v -> Scan z v2
transform f (Scan k) = Scan (fmap contFunctor f k)

mapScan : (a -> b) -> Scan z a -> Scan z b
mapScan f = transform (map_V f)

filterK : (k -> Bool) -> Scan z (k,v) -> Scan z (k,v)
filterK f = transform (filter_V (f . fst))

filterV : (v -> Bool) -> Scan z (k,v) -> Scan z (k,v)
filterV f = transform (filter_V (f . snd))

mapK : (k -> k2) -> Scan z (k,v) -> Scan z (k2,v)
mapK f = transform (map_V ((k,v) -> (f k, v)))

mapV : (v -> v2) -> Scan z (k,v) -> Scan z (k,v2)
mapV f = transform (map_V ((k,v) -> (k, f v)))

updateK : Eq k => k -> (v -> v) -> Scan z (k, v) -> Scan z (k, v)
updateK k f s = transform (map_V (kv -> if (fst kv == k) ' (k, f (snd kv)) ' kv)) s

sort : Ord v -> Scan z v -> Scan z v
sort o = transform (sort_V o)

sortK : Ord k -> Scan z (k, v) -> Scan z (k, v)
sortK o = transform (sortBy_V fst o)

sortV : Ord v -> Scan z (k, v) -> Scan z (k, v)
sortV o = transform (sortBy_V snd o)

pick : List (v -> Bool) -> Scan z v -> Scan z v
pick fs s = transform (pick_V fs) s

pickBy : Eq k => (v -> k) -> List k -> Scan z v -> Scan z v
pickBy f ks = pick (map_L (k t -> k == f t) ks)

pickK : Eq k => List k -> Scan z (k, v) -> Scan z (k, v)
pickK ks = pickBy fst ks

removeK : Eq k => k -> Scan z (k, v) -> Scan z (k, v)
removeK = removeBy fst

removeBy : Eq k => (v -> k) -> k -> Scan z v -> Scan z v
removeBy f kill s = transform ' filter_V (v -> f v != kill) ' s

deriveFromK : Eq k => Int -> k -> k -> (v -> v) -> Scan z (k, v) -> Scan z (k, v)
deriveFromK i k ko f s =
  let addEntry kvs kv = insert_V i (ko, f (snd kv)) kvs
      go kvs = maybe kvs (addEntry kvs) ' find_V ((k2,_) -> k2 == k) kvs
  in transform go s

pickByK : Eq k => (k -> k2) -> List k2 -> Scan z (k, v) -> Scan z (k, v)
pickByK f = pickBy (f . fst)

groupBy' : r <- (h,t) => RunScan List z -> Row h -> [..r] -> Scan z ({..h}, [..r])
groupBy' scanner row rel =
  fromRelation scanner (rel # row)
  |> transform (ks -> map_V (k -> (k, rel & k)) ks)

groupBy : r <- (h,t) => RunScan List z -> Row h -> [..r] -> Scan z ({..h}, [..t])
groupBy scanner row rel =
  groupBy' scanner row rel |> mapV (except row)

groupBy1 : r <- (h,t) => RunScan List z -> Field h k -> [..r] -> Scan z (k, [..t])
groupBy1 scanner f rel =
  groupBy scanner {f} rel |> mapK (t -> t ! f)

groupBy1' : r <- (h,t) => RunScan List z -> Field h k -> [..r] -> Scan z (k, [..r])
groupBy1' scanner f rel =
  groupBy' scanner {f} rel |> mapK (t -> t ! f)

sumBy' : (r <- (h,t), AsOp op, PrimitiveNum n)
      => op r n -> Field r2 n -> Scan z (k, [..r]) -> Scan z (k, [..r2])
sumBy' op f = mapV (aggregate_A (sum_A op) f)

sumBy : (r <- (h,t), AsOp op, PrimitiveNum n)
     => RunScan List z -> op r n -> Scan z (k, [..r]) -> Scan z (k, n)
sumBy runner op = case existentialF "value#" (typeOfOp op) of
  EField f -> multiply runner (getF f)
            . mapScan (mapSnd (aggregate_A (sum_A op) f))

avgBy' : (r <- (h,t), AsOp op, PrimitiveNum n)
     => op r n -> Field r2 n -> Scan z (k, [..r]) -> Scan z (k, [..r2])
avgBy' op f = mapV (aggregate_A (avg_A op) f)

avgBy runner op = case existentialF "value#" (typeOfOp op) of
  EField f -> multiply runner (getF f)
            . mapScan (mapSnd (aggregate_A (avg_A op) f))

count': (r <- (h,t), PrimitiveNum n)
     => Field r2 n -> Scan z (k, [..r]) -> Scan z (k, [..r2])
count' f = mapV (aggregate_A countAgg_A f)

count : r <- (h,t)
     => RunScan List z -> Scan z (k, [..r]) -> Scan z (k, Int)
count runner = case existentialF "value#" Int of
  EField f -> multiply runner (getF f)
            . mapScan (mapSnd (aggregate_A countAgg_A f))

-- | Product of all `k`s, remaining grouped together, with all results
-- derived from `f`.
multiply : RunScan List z -> ({..t} -> a) -> Scan z (k, [..t]) -> Scan z (k, a)
multiply runner f s = bind scanMonad s (strength scanFunctor . mapSnd extractF)
  where extractF = mapScan f . fromRelation runner

scanFunctor : Functor (Scan z)
scanFunctor = Functor mapScan

scanAp : Ap (Scan z)
scanAp = Ap unitScan apScan

scanMonad : Monad (Scan z)
scanMonad = Monad unitScan bindScan

private

  bindScan : Scan z a -> (a -> Scan z b) -> Scan z b
  bindScan m k = Scan (Cont (c -> runScan m (va ->
    runScan (traverse_V scanAp k va) (c . concatMap_V id))))

  unitScan : a -> Scan z a
  unitScan = Scan . unit contMonad . singleton_V

  apScan : Scan z (a -> b) -> Scan z a -> Scan z b
  apScan (Scan l) (Scan r) =
    Scan (liftA2 contAp (ap ap_V) l r)

  runScan : Scan z a -> ((Vector a -> z) -> z)
  runScan (Scan (Cont k)) = k

  adapt : RunScan List z -> RunScan Vector z
  adapt (RunScan scanner) = RunScan ' rel cb -> scanner rel (cb . vector_V)

  scan : Relational f => RunScan Vector z -> f r -> Scan z {..r}
  scan (RunScan scanner) r = Scan (Cont (scanner r))

{-
unify : Scan a -> Scan b -> (Scan a, Scan b)
keyValueTabular : Scan (k,[..v]) -> Report f z
-}

