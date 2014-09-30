module Map where

{- ^ scala.collection.immutable.SortedMap -}

import Native.Function
import Native.Object
import Native.Ord
import Native.Bool
import Native.List using {type List#; fromList#}
import Native.Maybe
import Native.Pair
import Function
import Maybe
import List
import Num
import Ord
import Pair
import IO using { catch; ioAp; ioFunctor }
import IO.Unsafe using unsafePerformIO
import Control.Ap
import Control.Functor
import Control.Monoid
import Vector as V
import Syntax.List
import String

pure_IO = pure ioAp
map_IO = fmap ioFunctor

foreign
  data "scala.collection.immutable.SortedMap" Map (k: *) (v: *)
  method "size" size : Map k v -> Int
  method "compare" compare# : Map k v -> k -> k -> Int

lookup : k -> Map k v -> Maybe v
lookup k m = fromMaybe# (lookup# m k)

lookupOr : v -> k -> Map k v -> v
lookupOr v k m = orElse v ' lookup k m

infix 5 !
(!) : Map k v -> k -> v
(!) m k = let msg = "Key not found: " ++ (toString k)
          in getJust msg (lookup k m)

member : k -> Map k v -> Bool
member k = isJust . lookup k

fromAssocList : Ord k -> List (k,v) -> Map k v
fromAssocList f = foldr (uncurry insert) (empty f)

toAssocList : Map k v -> List (k, v)
toAssocList = map fromPair# . fromList# . toList#

-- | Alias for !.
toFunc = (!)

toOrd : Map k v -> Ord k
toOrd m = fromCompare (compare# m)

orderList : Ord a -> List a -> Map a Int
orderList f xs = let integers = unfoldr (a -> Just (a, a + 1) ) 1
  in fromAssocList f $ zip xs integers

firstKey : Map k v -> Maybe k
firstKey m = unsafePerformIO <|
  catch (map_IO Just (firstKey# m)) (t -> pure_IO Nothing)

maybeFirstKey : a -> (k -> a) -> Map k v -> a
maybeFirstKey z f = maybe z f . firstKey

firstKeyOr : k -> Map k v -> k
firstKeyOr k = orElse k . firstKey

firstValue : Map k v -> Maybe v
firstValue m = maybe Nothing (Just . toFunc m) (firstKey m)

firstEntry : Map k v -> Maybe (k, v)
firstEntry m = case firstKey m of
  Just k -> Just (k, m ! k)
  _ -> Nothing

insert : k -> v -> Map k v -> Map k v
insert k v m = insert# m k v

-- | Remove by key, if present.
delete : k -> Map k v -> Map k v
delete k = MapIsObject . flip delete# k

-- | Change key.  If `f` is not an injection, then arbitrary keys will
-- win.
mapKeys : Ord k2 -> (k -> k2) -> Map k v -> Map k2 v
mapKeys o f m = fromAssocList o . map (mapFst f) . toAssocList $ m

empty : Ord k -> Map k v
empty = empty# sortedMapModule . toOrdering#

groupBy : Ord k -> (a -> k) -> Vector_V a -> Map k (Vector_V a)
groupBy o kf xs = groupBy# xs (function1 kf) (toOrdering# o)

-- | Left-biased union.
union : Map k v -> Map k v -> Map k v
union l r = foldl (flip . uncurry $ insert) r (toAssocList l)

-- | Lifted value semigroup.
unionWith : (v -> v -> v) -> Map k v -> Map k v -> Map k v
unionWith f l r = unionWith# l r (function2 f)

-- | A map monoid derived from semigroup value.
valueMonoid : Ord k -> (v -> v -> v) -> Monoid (Map k v)
valueMonoid k v = Monoid (empty k) (unionWith v)

-- | Maps form a functor over values.
valueFunctor : Functor (Map k)
valueFunctor = Functor (flip map# . function1)

private
  toOrdering# : Ord a -> OrdScala# a
  toOrdering# = ordScala# . ord#

private foreign
  subtype MapIsObject : Object -> Map k v
  data "scala.collection.immutable.SortedMap$" SortedMapModule
  value "scala.collection.immutable.SortedMap$" "MODULE$"
      sortedMapModule : SortedMapModule
  method "empty" empty# : SortedMapModule -> OrdScala# k -> Map k v
  method "updated" insert# : Map k v -> k -> v -> Map k v
  method "$minus" delete# : Map k v -> k -> Object -- scala.collection is weird
  method "firstKey" firstKey# : Map k v -> IO k
  method "get" lookup# : Map k v -> k -> Maybe# v
  method "toList" toList# : Map k v -> List# (Pair# k v)
  function "com.clarifi.reporting.ermine.session.foreign.SortedMap"
    "map" map# : Map k v -> (Function1 v v') -> Map k v'
  function "com.clarifi.reporting.ermine.session.foreign.SortedMap"
    "groupBy" groupBy# : Vector_V v -> (Function1 v k) -> OrdScala# k
                      -> Map k (Vector_V v)
  function "com.clarifi.reporting.ermine.session.foreign.SortedMap"
    "unionWith" unionWith# : Map k v -> Map k v -> (Function2 v v v) -> Map k v
