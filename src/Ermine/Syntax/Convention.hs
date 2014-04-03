{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveDataTypeable #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett 2014
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Ermine.Syntax.Convention
  ( Convention(..)
  , strict
  ) where

import Control.Applicative
import qualified Data.Binary as Binary
import Data.Binary (Binary)
import Data.Bytes.Serial
import Data.Data
import Data.Hashable
import qualified Data.Serialize as Serialize
import Data.Serialize (Serialize)
import GHC.Generics
import Prelude

-- | calling conventions for applications and lambdas
data Convention
  = C -- lazy core appliction
  | U -- strict unboxed value
  | D -- strict dictionary application
  | N -- strict native value application (for strings and foreign java objects)
  deriving (Eq,Ord,Show,Read,Enum,Bounded,Typeable,Data,Generic)

instance Hashable Convention where
  hashWithSalt n i = n `hashWithSalt` fromEnum i

instance Serial Convention where
  serialize = serialize . fromEnum
  deserialize = toEnum <$> deserialize

instance Binary Convention where
  put = serialize
  get = deserialize

instance Serialize Convention where
  put = serialize
  get = deserialize

strict :: Convention -> Bool
strict C = False
strict D = True
strict U = True
strict N = True


