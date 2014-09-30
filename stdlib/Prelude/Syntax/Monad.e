module Syntax.Monad where

import Syntax.List using map
import Syntax.Do
import Control.Monad using type Monad
import Function
import List

infixl 4 >>= >> << <*> <$ <$>

(>>=) = bind
(>>) = lift2 (x y -> y)
(<<) = lift2 (x y -> x)
(<*>) = lift2 ($)
(<$>) f a = bind a (unit . f)

lift2 f fa fb = do
  a <- fa; b <- fb
  unit (f a b)

lift3 f fa fb fc = do
  a <- fa; b <- fb; c <- fc
  unit (f a b c)

lift4 f fa fb fc fd = do
  a <- fa; b <- fb; c <- fc; d <- fd
  unit (f a b c d)

lift5 f fa fb fc fd fe = do
  a <- fa; b <- fb; c <- fc; d <- fd; e <- fe
  unit (f a b c d e)

sequenceList [] = unit []
sequenceList (h::t) = lift2 (::) h (sequenceList t)

sequenceList_ [] = unit ()
sequenceList_ (h::t) = h >> sequenceList_ t

traverseList f = sequenceList . map f

for_ f = sequenceList_ . map f
