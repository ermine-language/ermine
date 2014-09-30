module List.Stream where

{- ^ Streams are infinite lists, or lists without Nil.  They are
isomorphic to Cofree Id. -}

import Bool
import Function
import Control.Ap using type Ap; Ap
import Control.Functor
import Control.Monad.State
import Control.Monad using monadAp
import Control.Monoid
import Control.Traversable
import List as L
import Maybe
import Num
import Pair

infixr 5 <:>
data Stream a = Cons a (Stream a)

-- | Alias for stream constructor.
(<:>) = Cons

-- | Primitive stream fold.
foldr : (a -> b -> b) -> Stream a -> b
foldr f (Cons a as) = f a (foldr f as)

-- | Primitive stream generation, left to right.
unfoldr : (s -> (a, s)) -> s -> Stream a
unfoldr f = fix (rec -> (~(a, s) -> a <:> rec s) . f)

-- | Prepend a list to a stream.
append : List a -> Stream a -> Stream a
append = flip (foldr_L Cons)

-- | Repeat the a forever.
repeat : a -> Stream a
repeat = fix . Cons

-- | Repeat the as forever.
cycle : List a -> Stream a
cycle = fix . append

-- | Natural to infinite list.
toList : Stream a -> List a
toList = foldr (::)

-- | The first N elements of the stream.
take : Int -> Stream a -> List a
take n xs = unfoldr_L ((n, (Cons x xs)) ->
                         if (n > 0) (Just (x, (n - 1, xs))) Nothing)
                      (n, xs)

-- | Remove N elements.
drop : Int -> Stream a -> Stream a
drop n here@(Cons _ xs) = if (n > 0) (drop (n - 1) xs) here

-- | Remove elements until the predicate fails.
dropWhile : (a -> Bool) -> Stream a -> Stream a
dropWhile p here@(Cons x xs) = if (p x) (dropWhile p xs) here

-- | Yield [a, f a, f (f a), ...]
iterate : (a -> a) -> a -> Stream a
iterate f = unfoldr (a -> (a, f a))

-- | Remove non-matching elements.
filter : (a -> Bool) -> Stream a -> Stream a
filter p = foldr (x -> if (p x) (Cons x) id)

functor : Functor Stream
functor = Functor (f -> foldr (Cons . f))

-- | The zipping Ap instance, the only one that makes sense for
-- Stream.
ap : Ap Stream
ap = Ap repeat ap'
  where ap' (Cons f fs) (Cons x xs) = f x <:> ap' fs xs

-- | Join a stream to any traversable.
zipWith : Traversable tr -> (a -> b -> c) -> Stream a -> tr b -> tr c
zipWith tr f xs trb = fst (runState (traverse tr (monadAp stateMonad) ann trb) xs)
  where ann b = State ((Cons x xs) -> (f x b, xs))
