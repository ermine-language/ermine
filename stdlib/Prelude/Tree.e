module Tree where

import Vector as V
import List as L
import List.Stream as Inf
import Map as M
import Function
import Control.Functor
import Control.Ap
import Control.Traversable
import Control.Monad as M
import Control.Monad.Cont
import Pair
import Primitive
import Ord
import Maybe
import Eq
import Unsafe.Coerce
import Control.Monoid
import Relation.Scan as S hiding fromList
import Relation.Row
import Relation using relation
import Field
import Record as R
import Syntax.List as SL
import String as String

import Native.Function
import Native.List as L
import Native.Stream as S
import Native.Bool
import Native.Object
import Function
import Bool
import Vector as V

import Error

foreign
  data "scalaz.Tree" Tree a
  method "rootLabel" root : Tree a -> a
  method "subForest" children : Tree a -> Stream (Tree a)
  method "flatten" flatten : Tree a -> Stream a
  function "com.clarifi.reporting.ermine.session.foreign.Tree" "leaf" leaf : a -> Tree a
  function "com.clarifi.reporting.ermine.session.foreign.Tree" "node" nodeS : a -> Stream (Tree a) -> Tree a

zipWith = zipWith# . function2
map = map# . function1
zip = zipWith (,)
node rt cs = nodeS rt (fromList_S cs)

-- Tree.toString = "<tree>".  But sometimes we want to see that data.
toString' t = "Tree( "
  ++_String toString (root t)
  ++_String foldr_S
              (t' acc -> acc ++_String ", " ++_String toString' t')
              ""
              (children t)
  ++_String ")"

private foreign
  function "com.clarifi.reporting.ermine.session.foreign.Tree" "zipWith"
    zipWith# : Function2 a b c -> Tree a -> Tree b -> Tree c
  function "com.clarifi.reporting.ermine.session.foreign.Tree" "map"
    map# : Function1 a b -> Tree a -> Tree b
  function "com.clarifi.reporting.ermine.session.foreign.Tree" "scan"
    scan# : Function2 a (Stream b) b -> Tree a -> Tree b

-- BUSTED: join : c <- (a,b) => Tree {..a} -> Tree {..b} -> Tree {..c}
join : c1 <- (a1,b1) => Tree {..a1} -> Tree {..b1} -> Tree {..c1}
join t1 t2 = zipWith (++_R) t1 t2

scan : (a -> (List b) -> b) -> Tree a -> Tree b
scan f = scan# ' function2 (a s -> f a ' toList_S s)

unfold : (b -> (a, List b)) -> b -> Tree a
unfold f = fix (rec -> ((a, bs) -> node a (map_SL rec bs)) . f)

fold : (a -> List b -> b) -> Tree a -> b
fold f t = f (root t) (map_SL (fold f) . toList_S . children $ t)

fromRel : (Primitive n, Relational rel, t <- (pid,cid,h)) => RunScan List z -> n -> Field pid n -> Field cid n -> rel t -> Cont z (Tree {..h})
fromRel runner rootParentId parent child rel =
    consume_S (treeify . groupBy_M primOrd (getF parent)) $ fromRelation_S runner rel
  where treeify byPids =
          let tcs t = toList_V . lookupOr_M (vector_V []) (t ! child) $ byPids
          in unfold (t -> (exceptT {parent, child} t, tcs t))
                    (at_V 0 . getJust "nonexistent parent id in Tree.fromRel" $ lookup_M rootParentId byPids)

toRel : s <- (pid,cid,r) => n -> Field pid n -> Field cid n
                         -> Tree (n, {..r}) -> [..s]
toRel fakeroot pid cid = relation . flip id fakeroot . fold flat
  where flat (n, rtup) cs p = (rtup ++_R {pid = p, cid = n}) :: (cs >>=_SL flip id n)

-- | n must be a zero, and inc a +1.  XXX SMRC This is one of the
-- PrimitiveNum issues.
toRootedRel : (s <- (pid,cid,r), PrimitiveNum n)
           => n -> (n -> n) -> Field pid n -> Field cid n
           -> Tree {..r} -> [..s]
toRootedRel z inc pid cid = toRel z pid cid . identify (iterate_Inf inc (inc z))

identify : Stream_Inf b -> Tree a -> Tree (b, a)
identify = zipWith_Inf traversable (,)

aggregate : (n -> n -> n) -> (a -> n) -> Tree a -> Tree (n, a)
aggregate b f = scan (r cs -> (foldl_L b (f r) (map_SL fst cs), r))

aggregateLeaves : (n -> n -> n) -> (a -> Maybe n) -> Tree a -> Tree (n, a)
aggregateLeaves b f = fillZero . (aggregate . mappend . semigroupMonoid) b f

-- | Aggregations for which data to be aggregated exists at every
-- node.  XXX SMRC nulls for nullable n are viral
sum, minA, maxA {-, avg, etc-} : PrimitiveNum n => (a -> n) -> Tree a -> Tree (n, a)
sum = aggregate (+)
minA = aggregate (min primOrd)
maxA = aggregate (max primOrd)

-- | Aggregations for which data to be aggregated only exists at
-- leaves.
sumLeaves, minLeaves, maxLeaves : PrimitiveNum n => (a -> Maybe n) -> Tree a -> Tree (n, a)
sumLeaves = aggregateLeaves (+)
minLeaves = aggregateLeaves (min primOrd)
maxLeaves = aggregateLeaves (max primOrd)

traversable = Traversable trav

private
  trav : forall f a b . Ap f -> (a -> f b) -> Tree a -> f (Tree b)
  trav app f t = liftA2 app nodeS (f (root t)) ' traverse_S app (trav app f) (children t)
  traverse_S = traverse traversable_S
  sequence_S = sequenceA traversable_S
  map_Maybe = fmap maybeFunctor
  type Map = Map_M
  type RunScan = RunScan_S
  type Stream = Stream_S
  type Vector = Vector_S

  -- | We assume such trees are entirely filled.
  fillZero : Tree (Maybe n, a) -> Tree (n, a)
  fillZero = map (mapFst ' getJust "Tree (Nothing, a)")

-- toRecords : Field cid a -> Field pid a -> Tree a ->

private
{-
  accumulate : (a -> m) -> Monoid m -> Tree a -> Tree m
  accumulate f m t =

  acc : (a -> m) -> Monoid m -> Tree a -> (m, Tree m)
  acc f m@(Monoid z op) (Tree a cs) =
    let a = root t
        cs = children cs
        cs2 = map_SL ' acc f m ' toList_V cs
        r2 = foldl_L op z ' map_SL fst cs2
    in (r2, node r2 (vector_V . map_SL snd ' cs2))
-}

{-
fromIds : Primitive id => List (id, (id, a)) -> Tree a
fromIds ids =
  let toParent = fromAssocList_M primOrd ids
      toChildren = childIndices ids
      go rootid = case lookup_M rootid toParent of
        Nothing -> empty
        Just (parentid, a) -> node a '
          lookupOr_M empty rootid toChildren |> map_S go
  in maybe empty (go . fst) ' minBy_L fst ids

fromRecords : (r <- (pid,cid,t), Relational f, Primitive a)
          => RunScan List z -> Field pid a -> Field cid a -> f r -> Cont z (Tree {..t})
fromRecords scanner parent child rel =
  let ids = {parent, child}
      toTree ts = ts |> map_V (t -> (t ! child, (t ! parent, exceptT ids t)))
                     |> toList_V
                     |> fromIds
  in fromRelation_S scanner rel |> consume_S toTree

childIndices : Primitive id => List (id, (id, a)) -> Map id (Vector id)
childIndices l =
  let go acc [] = acc
      go acc ((nid, (pid, _)) :: t) = go ' snocV pid nid acc ' t
      snocV k v acc =
        insert_M k ' snoc_V v (lookupOr_M empty_V k acc) ' acc
  in go (empty_M primOrd) l

-}
