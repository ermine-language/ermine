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
-- >>> :set -XRank2Types
-- >>> import Control.Lens
-- >>> import Ermine
-- >>> let inferredKind :: (forall k t. Type k t) -> Either String (Schema a); inferredKind t = over _left snd $ runM Rendering $ generalize =<< inferKind t

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
-- Right (Schema 0 (Scope (HardKind Star)))
int :: Builtin t => t
int = builtin_ "Int"

-- |
-- >>> inferredKind long
-- Right (Schema 0 (Scope (HardKind Star)))
long :: Builtin t => t
long = builtin_ "Long"

-- |
-- >>> inferredKind byte
-- Right (Schema 0 (Scope (HardKind Star)))
byte :: Builtin t => t
byte = builtin_ "Byte"

-- |
-- >>> inferredKind short
-- Right (Schema 0 (Scope (HardKind Star)))
short :: Builtin t => t
short = builtin_ "Short"

-- |
-- >>> inferredKind float
-- Right (Schema 0 (Scope (HardKind Star)))
float :: Builtin t => t
float = builtin_ "Float"

-- |
-- >>> inferredKind double
-- Right (Schema 0 (Scope (HardKind Star)))
double :: Builtin t => t
double = builtin_ "Double"

------------------------------------------------------------------------------
-- Text
------------------------------------------------------------------------------

-- |
-- >>> inferredKind char
-- Right (Schema 0 (Scope (HardKind Star)))
char :: Builtin t => t
char = builtin_ "Char"

-- |
-- >>> inferredKind string
-- Right (Schema 0 (Scope (HardKind Star)))
string :: Builtin t => t
string = builtin_ "String"

------------------------------------------------------------------------------
-- Containers
------------------------------------------------------------------------------

-- |
-- >>> inferredKind list
-- Right (Schema 0 (Scope (HardKind Star :-> HardKind Star)))
--
-- >>> inferredKind (list int)
-- Right (Schema 0 (Scope (HardKind Star)))
list :: Builtin t => t
list = builtin (star ~> star) "List"

-- |
-- >>> inferredKind maybe_
-- Right (Schema 0 (Scope (HardKind Star :-> HardKind Star)))
--
-- >>> inferredKind (maybe_ int)
-- Right (Schema 0 (Scope (HardKind Star)))
maybe_ :: Builtin t => t
maybe_ = builtin (star ~> star) "Maybe"

-- |
-- >>> inferredKind (tup [int, list int])
-- Right (Schema 0 (Scope (HardKind Star)))
tup :: [Type k t] -> Type k t
tup xs = apps (tuple (length xs)) xs

-- |
-- >>> inferredKind equality
-- Right (Schema 1 (Scope (Var (B 0) :-> (Var (B 0) :-> HardKind Star))))
--
-- >>> inferredKind (equality equality)
-- Right (Schema 1 (Scope ((Var (B 0) :-> (Var (B 0) :-> HardKind Star)) :-> HardKind Star)))
equality :: Builtin t => t
equality = builtin ("a" ~> "a" ~> star) "Equality"

------------------------------------------------------------------------------
-- Classes
------------------------------------------------------------------------------

-- |
-- >>> inferredKind functor
-- Right (Schema 0 (Scope ((HardKind Star :-> HardKind Star) :-> HardKind Constraint)))
functor :: Builtin t => t
functor = builtin ((star ~> star) ~> constraint) "Functor"
