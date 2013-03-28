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

import Control.Monad
import qualified Data.Binary as Binary
import Data.Binary (Binary)
import Data.Bytes.Get
import Data.Bytes.Put
import Data.Bytes.Serial
import qualified Data.Serialize as Serialize
import Data.Serialize (Serialize)
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

instance Serial Literal where
  serialize (Int    i) = putWord8 0 >> serialize i
  serialize (Int64  i) = putWord8 1 >> serialize i
  serialize (Byte   i) = putWord8 2 >> serialize i
  serialize (Short  i) = putWord8 3 >> serialize i
  serialize (String i) = putWord8 4 >> serialize i
  serialize (Char   i) = putWord8 5 >> serialize i
  serialize (Float  i) = putWord8 6 >> serialize i
  serialize (Double i) = putWord8 7 >> serialize i

  deserialize = getWord8 >>= \b -> case b of
    0 -> liftM Int    deserialize
    1 -> liftM Int64  deserialize
    2 -> liftM Byte   deserialize
    3 -> liftM Short  deserialize
    4 -> liftM String deserialize
    5 -> liftM Char   deserialize
    6 -> liftM Float  deserialize
    7 -> liftM Double deserialize
    _ -> fail $ "deserialize Literal: unexpected constructor tag: " ++ show b

instance Binary Literal where
  put = serialize
  get = deserialize

instance Serialize Literal where
  put = serialize
  get = deserialize
