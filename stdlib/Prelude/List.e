module List where

import Eq
import Bool
import Pair
import Function
import Control.Monad
import Control.Monoid
import Control.Alt
import Control.Traversable
import Num
import Maybe hiding ord
import Ord as O
import Error

infixr 5 ++

singleton : a -> List a
singleton a = a :: Nil

unfoldr : (b -> Maybe (a,b)) -> b -> List a
unfoldr f z = case f z of
  Just(x,z') -> x :: unfoldr f z'
  Nothing -> Nil

iterate : (a -> a) -> a -> List a
iterate f a = a :: iterate f (f a)

repeat : a -> List a
repeat a = a :: t where t = repeat a

replicate : a -> Int -> List a
replicate x 0 = Nil
replicate x n = x :: replicate x (n - 1)

from : Int -> List Int
from n = n :: (from (n + 1))

-- [start .. end)
range : Int -> Int -> List Int
range start end = case start >= end of
  True -> Nil
  False -> start :: range (start + 1) end

foldr : (a -> b -> b) -> b -> List a -> b
foldr f z (x :: xs) = f x (foldr f z xs)
foldr f z [] = z

foldl : (b -> a -> b) -> b -> List a -> b
foldl f !z [] = z
foldl f !z (x :: xs) = foldl f (f z x) xs

-- | Reduction from the right.
foldr1 : (a -> a -> a) -> List a -> Maybe a
foldr1 _ [] = Nothing
foldr1 f xs = let fo' [x] = x
                  fo' (x :: xs) = f x $ fo' xs
              in Just (fo' xs)

-- | Reduction from the left.
foldl1 : (a -> a -> a) -> List a -> Maybe a
foldl1 _ [] = Nothing
foldl1 f (x :: xs) = Just $ foldl f x xs

-- | Reduction to some monoid.
foldMap : Monoid b -> (a -> b) -> List a -> b
foldMap m f = foldr (mappend m . f) (mempty m)

scanl, scanl' : (b -> a -> b) -> b -> List a -> List b
scanl _ b [] = singleton b
scanl f b (x :: xs) = b :: scanl f (f b x) xs

scanl' _ !b [] = singleton b
scanl' f !b (x :: xs) = b :: scanl f (f b x) xs

(++) : List a -> List a -> List a
(++) xs ys = foldr (::) ys xs

reverse : List a -> List a
reverse = foldl (flip (::)) Nil

head : List a -> a
head (x :: _) = x

tail : List a -> List a
tail (_ :: xs) = xs

init : List a -> List a
init Nil = error "init []"
init (h :: t) =
  let init' _ Nil = Nil
      init' h (h2 :: t2) = h :: init' h2 t2
  in init' h t

headOption : List a -> Maybe a
headOption (x :: _) = Just x
headOption _ = Nothing

headOrElse : List a -> a -> a
headOrElse (x :: _) _ = x
headOrElse _ a = a

maybeHead : b -> (a -> b) -> List a -> b
maybeHead _ f (h :: _) = f h
maybeHead b _ _ = b

uncons : List a -> Maybe (a, List a)
uncons (h :: t) = Just (h, t)
uncons _ = Nothing

null : List a -> Bool
null [] = True
null _  = False

length : List a -> Int
length  = foldl (x _ -> x + 1) 0

isEmpty l = 0 == length l

sum, product : List Int -> Int
sum     = foldl (+) 0
product = foldl (*) 1

minBy : Primitive b => (a -> b) -> List a -> Maybe a
minBy f [] = Nothing
minBy f (h :: t) = Just . snd ' foldl (p@(mb, a) a2 ->
  let b2 = f a2
  in if (b2 < mb) (b2, a2) p) (f h, h) t

concatMap : (a -> List b) -> List a -> List b
concatMap f [] = Nil
concatMap f (x :: xs) = f x ++ concatMap f xs

concat : List (List a) -> List a
concat = foldr (++) Nil

tails, inits, powerslice : List a -> List (List a)
tails xs = xs :: case xs of
  [] -> Nil
  _ :: xs' -> tails xs'

inits xs = Nil :: case xs of
  [] -> Nil
  x :: xs' -> map ((::) x) (inits xs')

powerslice xs = Nil :: concatMap (ys -> tail (inits ys)) (tails xs)

any, every : (a -> Bool) -> List a -> Bool
any p [] = False
any p (x :: xs) = p x || any p xs

every p [] = True
every p (x :: xs) = p x && every p xs

contains : Eq a => a -> List a -> Bool
contains a = any (x -> x == a)

and, or : List Bool -> Bool
and [] = True
and (x :: xs) = x && and xs

or [] = False
or (x :: xs) = x || or xs

span, break : forall a. (a -> Bool) -> List a -> (List a, List a)
span _ xs@[] = (xs, xs)
span p xs@(x :: xs') = if (p x)
  (let yzs = span p xs' in (x :: fst yzs,snd yzs))
  (Nil,xs)

break p = span (not . p)

-- | Split the input list, stopping runs at the first failure of the
-- given monadic predicate, running left to right.
spanM : Monad m -> (a -> m Bool) -> List a -> m (List a, List a)
spanM m p = foldr prepend (unit m (Nil, Nil)) . tails
  where prepend [] mxs = mxs
        prepend xs@(x :: _) mxs =
          ifM m (p x) (liftM m (mapFst $ (::) x) mxs)
                      (unit m (Nil, xs))

find : forall a. (a -> Bool) -> List a -> Maybe a
find _ [] = Nothing
find p (x::xs) = if (p x) (Just x) (find p xs)

take : Int -> List a -> List a
take n [] = Nil
take n (x :: xs) = if (n == 0) Nil (x :: (take (n - 1) xs))

at : Int -> List a -> Maybe a
at i (h :: t) = if (i <= 0) (Just h) (at (i - 1) t)
at i [] = Nothing

zip : List a -> List b -> List (a,b)
zip xs ys = zipWith (x y -> (x, y)) xs ys

zipIndex : List a -> List (Int, a)
zipIndex xs = zip (from 0) xs

indices : List a -> List Int
indices xs =
  let go [] _ = Nil
      go (_ :: t) n = n :: go t (n + 1)
  in go xs 0

splitAt : Int -> List a -> (List a, List a)
splitAt n xs = (take n xs, drop n xs)

pick : List Int -> List a -> List a
pick inds xs = concat (map (i -> maybe Nil singleton (at i xs)) inds)

filter : (a -> Bool) -> List a -> List a
filter f = foldr (h t -> if (f h) (h :: t) t) Nil

--| maps over the list, ignoring elements that map to `Nothing`
collect : (a -> Maybe b) -> List a -> List b
collect _ [] = Nil
collect p (a :: as) = case p a of
  Just b -> b :: collect p as
  Nothing -> collect p as

drop : Int -> List a -> List a
drop 0 xs = xs
drop n [] = Nil
drop n (x :: xs) = drop (n - 1) xs

zipWith : (a -> b -> c) -> List a -> List b -> List c
zipWith f (x :: xs) (y :: ys) = (f x y) :: (zipWith f xs ys)
zipWith f _ _ = Nil

fenceposts : List (a,a) -> List a
fenceposts [] = Nil
fenceposts ((h1,h2) :: t) = h1 :: h2 :: map snd t

-- | The 'intersperse' function takes an element and a list and
-- \`intersperses\' that element between the elements of the list.
-- For example,
--
-- > intersperse ',' "abcde" == "a,b,c,d,e"
intersperse             : a -> List a -> List a
intersperse _   []      = Nil
intersperse sep (x::xs) = x :: prependToAll sep xs

-- Not exported:
-- We want to make every element in the 'intersperse'd list available
-- as soon as possible to avoid space leaks. Experiments suggested that
-- a separate top-level helper is more efficient than a local worker.
prependToAll            : a -> List a -> List a
prependToAll _   []     = Nil
prependToAll sep (x::xs) = sep :: x :: prependToAll sep xs

ord : (a -> a -> Ordering_O) -> List a -> List a -> Ordering_O
ord _ [] [] = EQ_O
ord _ _  [] = GT_O
ord _ [] _  = LT_O
ord f (x :: xs) (y :: ys) = case f x y of
  EQ_O -> ord f xs ys
  o  -> o

listLt : (a -> a -> Bool) -> List a -> List a -> Bool
listLt = lt_O . ord . fromLess_O

listFunctor = Functor map

private map : forall a b. (a -> b) -> List a -> List b
map f (x :: xs) = f x :: map f xs
map _ [] = Nil

map_List = map

listAp = Ap singleton ap'
private
  ap' : List (a -> b) -> List a -> List b
  ap' fs as = concatMap (flip map as) fs

listMonad = Monad singleton (flip concatMap)

listTraversable = Traversable traverse'
private
  traverse' : Ap f -> (a -> f b) -> List a -> f (List b)
  traverse' f _ []        = pure f Nil
  traverse' f g (x :: xs) = liftA2 f (::) (g x) (traverse' f g xs)

listAlt = Alt Nil (++) listAp

empty_Bracket = Nil
cons_Bracket = (::)
