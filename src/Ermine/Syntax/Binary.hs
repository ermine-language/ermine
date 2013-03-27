--------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett, Dan Doel and Josh Cough 2013
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable (DeriveDataTypeable)
--
--------------------------------------------------------------------
module Ermine.Syntax.Binary
  ( putMany
  , getMany
  ) where

import Control.Applicative
import Control.Monad
import Data.Foldable
import Data.Binary

putMany :: (k -> Put) -> [k] -> Put
putMany p ls = put (length ls) *> traverse_ p ls
{-# INLINE putMany #-}

getMany :: Get k -> Get [k]
getMany g = get >>= \n -> replicateM n g
{-# INLINE getMany #-}
