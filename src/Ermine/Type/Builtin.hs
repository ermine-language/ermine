{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
--------------------------------------------------------------------
-- |
-- Module    :  Ermine.Type.Builtin
-- Copyright :  (c) Edward Kmett and Dan Doel 2012
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
-- This module supplies a number of builtin types intrinsic to the
-- Ermine language.
--
-- >>> inferredKind int
-- *
--
-- >>> inferredKind (list list)
-- *** Exception: (interactive):1:1: error: kind mismatch
--------------------------------------------------------------------
module Ermine.Type.Builtin
  (
  -- * Builtin Types
    Builtin(..)
  , builtin_
  -- ** Numerics
  , int, long, byte, short
  , float, double
  -- ** Text
  , string, char
  -- ** Containers
  , list
  , maybe_
  , tup
  -- ** Type Equality
  , equality
  -- ** Classes
  , functor
  ) where

import Data.ByteString.Char8 hiding (foldl, length, reverse)
import Ermine.Global
import Ermine.Kind
import Ermine.Type
import Ermine.Syntax

-- $setup
-- >>> :set -XRank2Types -XNoMonomorphismRestriction -XExtendedDefaultRules
-- >>> :m + Control.Lens Data.Void Ermine Text.Trifecta.Rendering System.Console.Terminfo.PrettyPrint
-- >>> import Ermine.Pretty (names)
-- >>> let showSchema :: Schema String -> TermDoc; showSchema s = prettySchema s names prettyTerm
-- >>> let inferredKind :: (forall k t. Type k t) -> TermDoc; inferredKind t = showSchema $ runM_ emptyRendering $ generalize =<< inferKind t

------------------------------------------------------------------------------
-- Builtin
------------------------------------------------------------------------------

-- | This class allows for overloading of builtin types, so they can be used directly a nice DSL for specifying types
-- borrowing application from Haskell for the application of types.
class Builtin t where
  builtin :: Ord k => Kind k -> String -> t

instance Builtin HardType where
  builtin s n = con (builtin s n) (general s)

instance Builtin Global where
  builtin _ n = global Idfix (pack "ermine") (pack "Prelude") (pack n)

instance Builtin (Type k t) where
  builtin s n = HardType (builtin s n)

class Builtin' x where
  type K x :: *
  type T x :: *
  builtin' :: Ord k => Kind k -> String -> [Type (K x) (T x)] -> x

instance (Builtin' y, x ~ Type (K y) (T y)) => Builtin' (x -> y) where
  type K (x -> y) = K y
  type T (x -> y) = T y
  builtin' s n xs x = builtin' s n (x:xs)

instance Builtin' (Type k a) where
  type K (Type k a) = k
  type T (Type k a) = a
  builtin' s n xs = apps (builtin s n) (reverse xs)

instance (Builtin' y, x ~ Type (K y) (T y)) => Builtin (x -> y) where
  builtin s n y = builtin' s n [y]

-- | Create a 'Builtin' 'Type' of 'Kind' 'star'.
builtin_ :: Builtin t => String -> t
builtin_  = builtin star

------------------------------------------------------------------------------
-- Numeric Types
------------------------------------------------------------------------------

-- |
-- >>> inferredKind int
-- *
int :: Builtin t => t
int = builtin_ "Int"

-- |
-- >>> inferredKind long
-- *
long :: Builtin t => t
long = builtin_ "Long"

-- |
-- >>> inferredKind byte
-- *
byte :: Builtin t => t
byte = builtin_ "Byte"

-- |
-- >>> inferredKind short
-- *
short :: Builtin t => t
short = builtin_ "Short"

-- |
-- >>> inferredKind float
-- *
float :: Builtin t => t
float = builtin_ "Float"

-- |
-- >>> inferredKind double
-- *
double :: Builtin t => t
double = builtin_ "Double"

------------------------------------------------------------------------------
-- Text
------------------------------------------------------------------------------

-- |
-- >>> inferredKind char
-- *
char :: Builtin t => t
char = builtin_ "Char"

-- |
-- >>> inferredKind string
-- *
string :: Builtin t => t
string = builtin_ "String"

------------------------------------------------------------------------------
-- Containers
------------------------------------------------------------------------------

-- |
-- >>> inferredKind list
-- * -> *
--
-- >>> inferredKind (list int)
-- *
list :: Builtin t => t
list = builtin (star ~> star) "List"

-- |
-- >>> inferredKind maybe_
-- * -> *
--
-- >>> inferredKind (maybe_ int)
-- *
maybe_ :: Builtin t => t
maybe_ = builtin (star ~> star) "Maybe"

-- |
-- >>> inferredKind (tup [int, list int])
-- *
tup :: [Type k t] -> Type k t
tup xs = apps (tuple (length xs)) xs

-- |
-- >>> inferredKind equality
-- a -> a -> *
--
-- >>> inferredKind (equality equality)
-- (a -> a -> *) -> *
equality :: Builtin t => t
equality = builtin ("a" ~> "a" ~> star) "Equality"

------------------------------------------------------------------------------
-- Classes
------------------------------------------------------------------------------

-- |
-- >>> inferredKind functor
-- (* -> *) -> Γ
--
-- >>> inferredKind (equality functor)
-- ((* -> *) -> Γ) -> *
functor :: Builtin t => t
functor = builtin ((star ~> star) ~> constraint) "Functor"
