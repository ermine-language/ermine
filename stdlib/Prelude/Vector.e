module Vector where

import Bool
import List as L
import Native.List using { type List# ; fromList# }
import Native.Pair
import Native.Maybe
import Native.Function using { type Function2; type Function1; function1; function2 }
import Native.Bool using { type Bool#; toBool# }
import Num
import Ord as Ord
import Control.Monad hiding ap
import Control.Traversable hiding traverse
import Function

type Ord = Ord_Ord

foreign
  data "scala.collection.immutable.Vector" Vector a
  function "com.clarifi.reporting.ermine.session.foreign.Vector" "isEmpty"
    isEmpty : Vector a -> Bool
  function "com.clarifi.reporting.ermine.session.foreign.Vector" "empty"
    empty : Vector a
  function "com.clarifi.reporting.ermine.session.foreign.Vector" "snoc"
    snoc : a -> Vector a -> Vector a
  function "com.clarifi.reporting.ermine.session.foreign.Vector" "cons"
    cons : a -> Vector a -> Vector a
  function "com.clarifi.reporting.ermine.session.foreign.Vector" "at"
    at : Int -> Vector a -> a
  function "com.clarifi.reporting.ermine.session.foreign.Vector" "drop"
    drop : Int -> Vector a -> Vector a
  function "com.clarifi.reporting.ermine.session.foreign.Vector" "take"
    take : Int -> Vector a -> Vector a
  function "com.clarifi.reporting.ermine.session.foreign.Vector" "append"
    append : Vector a -> Vector a -> Vector a
  method "length" length : Vector a -> Int

private foreign
  method "toList" toList# : Vector a -> List# a
  function "com.clarifi.reporting.ermine.session.foreign.Vector" "fold"
    reduce# : a -> (Function2 a a a) -> Vector a -> a
  function "com.clarifi.reporting.ermine.session.foreign.Vector" "zipWith"
    zipWith# : Function2 a b c -> Vector a -> Vector b -> Vector c
  function "com.clarifi.reporting.ermine.session.foreign.Vector" "sort"
    sort# : Function2 a a Bool# -> Vector a -> Vector a
  function "com.clarifi.reporting.ermine.session.foreign.Vector" "map"
    map# : Function1 a b -> Vector a -> Vector b
  function "com.clarifi.reporting.ermine.session.foreign.Vector" "filter"
    filter# : Function1 a Bool# -> Vector a -> Vector a
  function "com.clarifi.reporting.ermine.session.foreign.Vector" "concatMap"
    concatMap# : Function1 a (Vector b) -> Vector a -> Vector b
  function "com.clarifi.reporting.ermine.session.foreign.Vector" "find"
    find# : Function1 a Bool# -> Vector a -> Maybe# a

infixr 5 ++
infixl 6 !+

(!+) = flip snoc
(++) = append

toList : Vector a -> List a
toList vs = fromList# (toList# vs)

fromList = vector

vector : List a -> Vector a
vector xs =
  let go [] acc = acc
      go (h :: t) acc = go t (snoc h acc)
  in go xs empty

empty_Bracket = empty
cons_Bracket = cons

insert : Int -> a -> Vector a -> Vector a
insert ind a v = take ind v ++ cons a (drop ind v)

sort : Ord a -> Vector a -> Vector a
sort o v = sort# (function2 (x y -> toBool# (lt_Ord o x y))) v

sortBy : (a -> k) -> Ord k -> Vector a -> Vector a
sortBy f o = sort (contramap_Ord f o)

pick : List (a -> Bool) -> Vector a -> Vector a
pick fs v = concatMap (f -> filter f v) (vector fs)

filter : (a -> Bool) -> Vector a -> Vector a
filter f = filter# (function1 (x -> toBool# (f x)))

find : (a -> Bool) -> Vector a -> Maybe a
find f vs = fromMaybe# ' find# (function1 (x -> toBool# (f x))) vs

map : (a -> b) -> Vector a -> Vector b
map f = map# (function1 f)

foldl : (b -> a -> b) -> b -> Vector a -> b
foldl f z v = rec z 0
  where l = length v
        rec !z n = if (n >= l) z (rec (f z $ at n v) (n + 1))

zip : Vector a -> Vector b -> Vector (a,b)
zip = zipWith (,)

zipWith : (a -> b -> c) -> Vector a -> Vector b -> Vector c
zipWith f = zipWith# (function2 f)

concatMap : (a -> Vector b) -> Vector a -> Vector b
concatMap f = concatMap# (function1 f)

reduce : a -> (a -> a -> a) -> Vector a -> a
reduce z f = reduce# z (function2 f)

singleton : a -> Vector a
singleton a = snoc a empty

monad : Monad Vector
monad = Monad singleton (xs f -> concatMap f xs)

ap : Ap Vector
ap = monadAp monad

traversable = Traversable traverse

traverse : forall f a b . Ap f -> (a -> f b) -> Vector a -> f (Vector b)
traverse app f v = reduce (pure app empty) (liftA2 app append) (map g v)
  where g a = liftA app singleton (f a)
