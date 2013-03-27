{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett and Dan Doel 2012-2013
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Ermine.Syntax.Literal
  ( Literal(..)
  ) where

import Control.Applicative
import Data.Binary as Binary
import Data.Data hiding (Fixity(..))
import Data.Hashable
import Data.Int
import GHC.Generics

-- | Primitive literal values used by patterns, terms and core.
data Literal
  = Int     !Int
  | Int64   !Int64
  | Byte    !Int8
  | Short   !Int16
  | String  String
  | Char    !Char
  | Float   !Float
  | Double  !Double
  deriving (Eq,Ord,Show,Read,Data,Typeable,Generic)

instance Hashable Literal

instance Binary Literal where
  put (Int    i) = putWord8 0 *> put i
  put (Int64  i) = putWord8 1 *> put i
  put (Byte   i) = putWord8 2 *> put i
  put (Short  i) = putWord8 3 *> put i
  put (String i) = putWord8 4 *> put i
  put (Char   i) = putWord8 5 *> put i
  put (Float  i) = putWord8 6 *> put i
  put (Double i) = putWord8 7 *> put i

  get = getWord8 >>= \b -> case b of
    0 -> Int    <$> get
    1 -> Int64  <$> get
    2 -> Byte   <$> get
    3 -> Short  <$> get
    4 -> String <$> get
    5 -> Char   <$> get
    6 -> Float  <$> get
    7 -> Double <$> get
    _ -> fail $ "get Literal: unexpected constructor tag: " ++ show b

