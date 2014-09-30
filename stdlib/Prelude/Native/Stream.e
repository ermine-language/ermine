module Native.Stream where

import Native.Function
import Native.List as L
import Native.Bool
import Function
import Bool
import Control.Traversable
import Control.Ap

foreign
  data "scala.collection.immutable.Stream" Stream a
  function "com.clarifi.reporting.ermine.session.foreign.Stream" "isEmpty"
    isEmpty : Stream a -> Bool
  function "com.clarifi.reporting.ermine.session.foreign.Stream" "empty"
    empty : Stream a
  function "com.clarifi.reporting.ermine.session.foreign.Stream" "drop"
    drop : Int -> Stream a -> Stream a
  function "com.clarifi.reporting.ermine.session.foreign.Stream" "take"
    take : Int -> Stream a -> Stream a
  method "length" length : Stream a -> Int
  method "head" head : Stream a -> a
  method "tail" tail : Stream a -> Stream a
  method "toList" toList# : Stream a -> List#_L a

infixr 5 #::


cons h t = cons# h (delay t)
(#::) = cons

zipWith f = zipWith# . function2
filter = filter# . function1
map = map# . function1
concatMap = concatMap# . function1
toList s = if (isEmpty s) Nil ' head s :: toList (tail s)

foldr : (a -> b -> b) -> b -> Stream a -> b
foldr f seed s = if ' isEmpty s ' seed
                                ' f (head s) (foldr f seed ' tail s)

fromList Nil = empty
fromList (h :: t) = cons h (fromList t)

traversable = Traversable traverse'
private
  traverse' : Ap f -> (a -> f b) -> Stream a -> f (Stream b)
  traverse' f g s =
    if (isEmpty s) '
      pure f empty '
      liftA2 f (#::) (g ' head s) (traverse' f g ' tail s)

private foreign
  function "com.clarifi.reporting.ermine.session.foreign.Stream" "cons"
    cons# : a -> (Function0 (Stream a)) -> Stream a
  function "com.clarifi.reporting.ermine.session.foreign.Stream" "append"
    append : Stream a -> Function0 (Stream a) -> Stream a
  function "com.clarifi.reporting.ermine.session.foreign.Stream" "zipWith"
    zipWith# : Function2 a b c -> Stream a -> Stream b -> Stream c
  function "com.clarifi.reporting.ermine.session.foreign.Stream" "map"
    map# : Function1 a b -> Stream a -> Stream b
  function "com.clarifi.reporting.ermine.session.foreign.Stream" "filter"
    filter# : Function1 a Bool# -> Stream a -> Stream a
  function "com.clarifi.reporting.ermine.session.foreign.Stream" "flatMap"
    concatMap# : Function1 a (Stream b) -> Stream a -> Stream b
