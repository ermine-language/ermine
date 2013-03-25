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
module Ermine.Syntax.Literal (Literal(..)) where

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
