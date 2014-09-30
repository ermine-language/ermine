module Either where

import List
import Control.Monad
import Syntax.List

data Either a b = Left a | Right b

either : (a -> c) -> (b -> c) -> Either a b -> c
either f g (Left a) = f a
either f g (Right b) = g b

lefts : List (Either a b) ->  List a
lefts (Left a :: as) = a :: lefts as
lefts (_ :: as) = lefts as
lefts [] = []

rights : List (Either a b) -> List b
rights (Right a :: as) = a :: rights as
rights (_ :: as) = rights as
rights [] = []

eitherMaybe : Either a b -> Maybe b
eitherMaybe (Left a) = Nothing
eitherMaybe (Right b) = Just b

maybeEither : e -> Maybe a -> Either e a
maybeEither e (Just a) = (Right a)
maybeEither e _ = (Left e)

-- | Return `Right`, if either argument is `Right`,
-- otherwise return the last `Left`
orEither : Either a b -> Either a b -> Either a b
orEither l@(Right _) _ = l
orEither _ r = r

partitionEithers : List (Either a b) -> (List a, List b)
partitionEithers = let left  a (l, r) = (a::l, r)
                       right a (l, r) = (l, a::r)
                    in foldr (either left right) ([], [])

eitherFunctor : Functor (Either e)
eitherFunctor = Functor map'
private
  map' : (a -> b) -> Either e a -> Either e b
  map' f (Right a) = Right (f a)
  map' f (Left e) = (Left e)

eitherAp : Ap (Either e)
eitherAp = Ap Right ap'
private
  ap' : Either e (a -> b) -> Either e a -> Either e b
  ap' (Right f) (Right a) = Right (f a)
  ap' (Left e) _ = (Left e)
  ap' _ (Left e) = (Left e)

eitherMonad : Monad (Either e)
eitherMonad = Monad Right bind'
private
  bind' : Either e a -> (a -> Either e b) -> Either e b
  bind' (Right m) k = k m
  bind' (Left e) _ = (Left e)
