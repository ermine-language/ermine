{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett and Dan Doel 2012
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
-- This module supplies a number of builtin types intrinsic to the
-- Ermine language.
--
-- >>> infer int
-- *
--
-- >>> infer (list list)
-- *** Exception: (interactive):1:1: error: kind mismatch
--------------------------------------------------------------------
module Ermine.Builtin.Type
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
  -- ** Type Equality
  , equality
  -- ** Classes
  , functor
  ) where

import Data.Text (pack)
import Ermine.Syntax
import Ermine.Syntax.Global
import Ermine.Syntax.Hint
import Ermine.Syntax.Type as Type
import Ermine.Syntax.Kind as Kind

-- $setup
-- >>> :set -XRank2Types -XNoMonomorphismRestriction -XExtendedDefaultRules
-- >>> :m + Control.Lens Data.Void Ermine Text.Trifecta.Rendering Data.Functor.Identity Ermine.Pretty.Kind
-- >>> import Ermine.Pretty (names, plain, Doc, Pretty(..))
-- >>> let showSchema :: Schema String -> Doc; showSchema s = plain $ runIdentity $ prettySchema s names $ const . Identity . pretty
-- >>> let infer :: (forall k t. Type k t) -> Doc; infer t = showSchema $ runM_ emptyRendering $ generalize =<< inferKind t

------------------------------------------------------------------------------
-- Builtin
------------------------------------------------------------------------------

-- | This class allows for overloading of builtin types, so they can be used directly a nice DSL for specifying types
-- borrowing application from Haskell for the application of types.
class Builtin t where
  builtin :: Ord k => Kind k -> String -> t

instance Builtin HardType where
  builtin s n = con (builtin s n) (Kind.general s (const $ Unhinted ()))

instance Builtin Global where
  builtin _ n = glob Idfix (pack "ermine") (pack "Prelude") (pack n)

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
-- >>> infer int
-- *
int :: Builtin t => t
int = builtin_ "Int"

-- |
-- >>> infer long
-- *
long :: Builtin t => t
long = builtin_ "Long"

-- |
-- >>> infer byte
-- *
byte :: Builtin t => t
byte = builtin_ "Byte"

-- |
-- >>> infer short
-- *
short :: Builtin t => t
short = builtin_ "Short"

-- |
-- >>> infer float
-- *
float :: Builtin t => t
float = builtin_ "Float"

-- |
-- >>> infer double
-- *
double :: Builtin t => t
double = builtin_ "Double"

------------------------------------------------------------------------------
-- Text
------------------------------------------------------------------------------

-- |
-- >>> infer char
-- *
char :: Builtin t => t
char = builtin_ "Char"

-- |
-- >>> infer string
-- *
string :: Builtin t => t
string = builtin_ "String"

------------------------------------------------------------------------------
-- Containers
------------------------------------------------------------------------------

-- |
-- >>> infer list
-- * -> *
--
-- >>> infer (list int)
-- *
--
-- >>> infer (tup [int, list int])
-- *

list :: Builtin t => t
list = builtin (star ~> star) "List"

-- |
-- >>> infer maybe_
-- * -> *
--
-- >>> infer (maybe_ int)
-- *
maybe_ :: Builtin t => t
maybe_ = builtin (star ~> star) "Maybe"

-- |
-- >>> infer equality
-- a -> a -> *
--
-- >>> infer (equality equality)
-- (a -> a -> *) -> *
equality :: Builtin t => t
equality = builtin ("a" ~> "a" ~> star) "Equality"

------------------------------------------------------------------------------
-- Classes
------------------------------------------------------------------------------

-- |
-- >>> infer functor
-- (* -> *) -> Γ
--
-- >>> infer (equality functor)
-- ((* -> *) -> Γ) -> *
functor :: Builtin t => t
functor = builtin ((star ~> star) ~> constraint) "Functor"
