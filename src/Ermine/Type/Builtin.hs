{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
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

------------------------------------------------------------------------------
-- Builtin
------------------------------------------------------------------------------

class Builtin t where
  builtin :: Ord k => Kind k -> String -> t

instance Builtin HardType where
  builtin s n = con (global Idfix (pack "ermine") (pack "Prelude") (pack n)) (general s)

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

builtin_ :: Builtin t => String -> t
builtin_  = builtin star

------------------------------------------------------------------------------
-- Numeric Types
------------------------------------------------------------------------------

int, long, byte, short, float, double :: Builtin t => t
int    = builtin_ "Int"
long   = builtin_ "Long"
byte   = builtin_ "Byte"
short  = builtin_ "Short"
float  = builtin_ "Float"
double = builtin_ "Double"

------------------------------------------------------------------------------
-- Text
------------------------------------------------------------------------------

char, string :: Builtin t => t
char   = builtin_ "Char"
string = builtin_ "String"

------------------------------------------------------------------------------
-- Containers
------------------------------------------------------------------------------

list :: Builtin t => t
list = builtin (star ~> star) "List"

maybe_ :: Builtin t => t
maybe_ = builtin (star ~> star) "Maybe"

tup :: [Type k t] -> Type k t
tup xs = apps (tuple (length xs)) xs

equality :: Builtin t => t
equality = builtin ("a" ~> "a" ~> star) "Equality"

------------------------------------------------------------------------------
-- Classes
------------------------------------------------------------------------------

functor :: Builtin t => t
functor = builtin ((star ~> star) ~> constraint) "Functor"
