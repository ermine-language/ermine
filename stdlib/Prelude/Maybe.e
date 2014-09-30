module Maybe where

import Control.Monad
import Control.Monoid
import Control.Alt
import Control.Traversable
import Function
import Error
import Ord

maybe : a -> (b -> a) -> Maybe b -> a
maybe n _ Nothing  = n
maybe _ j (Just b) = j b

orElse : a -> Maybe a -> a
orElse a Nothing  = a
orElse _ (Just b) = b

orMaybe : Maybe a -> Maybe a -> Maybe a
orMaybe Nothing a = a
orMaybe (Just a) _ = Just a

isJust, isNothing : Maybe a -> Bool
isJust (Just _) = True
isJust _ = False

isNothing Nothing = True
isNothing _ = False

getJust : String -> Maybe a -> a
getJust msg = orElse (error msg)

listToMaybe : List a -> Maybe a
listToMaybe (a :: _) = Just a
listToMaybe [] = Nothing

maybeToList : Maybe a -> List a
maybeToList Nothing = Nil
maybeToList (Just a) = a :: Nil

catMaybes : List (Maybe a) -> List a
catMaybes (Nothing :: as) = catMaybes as
catMaybes (Just a :: as) = a :: catMaybes as
catMaybes _ = Nil

-- | Adding a disjoint Nothing to a semigroup yields a monoid.
semigroupMonoid : (a -> a -> a) -> Monoid (Maybe a)
semigroupMonoid app = Monoid Nothing alt'
  where alt' (Just l) (Just r) = Just (app l r)
        alt' l' r' = orMaybe l' r'

maybeFunctor : Functor Maybe
maybeFunctor = Functor map'
private
  map' : (a -> b) -> Maybe a -> Maybe b
  map' f (Just a) = Just (f a)
  map' f Nothing = Nothing

maybeAp : Ap Maybe
maybeAp = Ap Just ap'
private
  ap' : Maybe (a -> b) -> Maybe a -> Maybe b
  ap' (Just f) (Just a) = Just (f a)
  ap' _ _ = Nothing

maybeAlt = Alt Nothing plus' maybeAp
private
  plus' Nothing b = b
  plus' a _       = a

maybeMonad : Monad Maybe
maybeMonad = Monad Just bind'
private
  bind' : Maybe a -> (a -> Maybe b) -> Maybe b
  bind' Nothing _ = Nothing
  bind' (Just a) f = f a

maybeTraversable : Traversable Maybe
maybeTraversable = Traversable traverse'
private
  traverse' : Ap f -> (a -> f b) -> Maybe a -> f (Maybe b)
  traverse' f g Nothing  = pure f Nothing
  traverse' f g (Just a) = liftA f Just (g a)

ord : (a -> a -> Ordering) -> Maybe a -> Maybe a -> Ordering
ord _ Nothing  Nothing  = EQ
ord _ Nothing  _        = LT
ord _ _        Nothing  = GT
ord f (Just a) (Just b) = f a b
