{-# LANGUAGE DeriveDataTypeable #-}
--------------------------------------------------------------------
-- |
-- Module    :  Ermine.Prim
-- Copyright :  (c) Edward Kmett and Dan Doel 2012
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--------------------------------------------------------------------
module Ermine.Prim
  ( Prim(..)
  , prim
  ) where

import Data.ByteString.Char8
import Data.Data hiding (Fixity(..))
import Data.Int
import Ermine.Global

data Prim
  = DataCon !Global -- Foo
  | Tuple   !Int    -- (,,)
  | Int     !Int
  | Int64   !Int64
  | Byte    !Int8
  | Short   !Int16
  | String  String
  | Char    !Char
  | Float   !Float
  | Double  !Double
  deriving (Eq,Ord,Show,Data,Typeable)

prim :: Fixity -> String -> String -> Prim
prim f m n = DataCon (global f (pack "ermine") (pack m) (pack n))
