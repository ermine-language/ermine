{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveDataTypeable #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) Dan Doel 2013
-- License   :  BSD3
-- Maintainer:  Dan Doel <dan.doel@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--------------------------------------------------------------------
module Ermine.Syntax.Hint
  ( Hinted(..)
  , Hint
  , hint
  ) where

import Control.Comonad
import Data.Bytes.Serial
import qualified Data.Binary as Binary
import Data.Binary (Binary)
import Data.Data
import Data.Function
import Data.Hashable
import Data.Hashable.Extras
import qualified Data.Serialize as Serialize
import Data.Serialize (Serialize)
import GHC.Generics
import Prelude.Extras

data Hinted a = Unhinted a
              | Hinted String a
  deriving (Show, Read, Functor, Typeable, Data, Generic, Generic1)

type Hint = Hinted ()

instance Comonad Hinted where
  extract (Unhinted a) = a
  extract (Hinted _ a) = a

  extend f h@(Unhinted _) = Unhinted (f h)
  extend f h@(Hinted s _) = Hinted s (f h)

hint :: Hinted a -> Maybe String
hint (Hinted s _) = Just s
hint _            = Nothing

instance Eq1 Hinted
instance Eq a => Eq (Hinted a) where
  (==) = (==) `on` extract

instance Ord1 Hinted
instance Ord a => Ord (Hinted a) where
  compare = compare `on` extract

instance Serial1 Hinted
instance Serial a => Serial (Hinted a) where
  serialize = serialize1 ; deserialize = deserialize1

instance Binary a => Binary (Hinted a) where
  put = serializeWith Binary.put ; get = deserializeWith Binary.get

instance Serialize a => Serialize (Hinted a) where
  put = serializeWith Serialize.put ; get = deserializeWith Serialize.get

instance Hashable a => Hashable (Hinted a)
instance Hashable1 Hinted
