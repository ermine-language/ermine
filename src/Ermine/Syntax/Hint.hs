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
  , stringHint
  , maybeHint
  ) where

import Control.Applicative
import Control.Comonad
import Control.Lens
import Data.Bytes.Get
import Data.Bytes.Put
import Data.Bytes.Serial
import qualified Data.Binary as Binary
import Data.Binary (Binary)
import Data.Data
import Data.Foldable
import Data.Function
import Data.Hashable
import Data.Hashable.Extras
import Data.Text (Text)
import qualified Data.Serialize as Serialize
import Data.Serialize (Serialize)
import Ermine.Syntax.Digest
import GHC.Generics
import Prelude.Extras

data Hinted a = Unhinted a
              | Hinted Text a
  deriving (Show,Read,Functor,Foldable,Traversable,Typeable,Data,Generic)

type Hint = Hinted ()

instance Comonad Hinted where
  extract (Unhinted a) = a
  extract (Hinted _ a) = a

  extend f h@(Unhinted _) = Unhinted (f h)
  extend f h@(Hinted s _) = Hinted s (f h)

hint :: Hinted a -> Maybe Text
hint (Hinted s _) = Just s
hint _            = Nothing

stringHint :: Text -> Hint
stringHint = Hinted ?? ()

maybeHint :: Maybe Text -> Hint
maybeHint = maybe (Unhinted ()) stringHint

instance Eq1 Hinted
instance Eq a => Eq (Hinted a) where
  (==) = (==) `on` extract

instance Ord1 Hinted
instance Ord a => Ord (Hinted a) where
  compare = compare `on` extract

instance Serial1 Hinted where
  serializeWith f (Unhinted a) = putWord8 0 >> f a
  serializeWith f (Hinted s a) = putWord8 1 >> serialize s >> f a

  deserializeWith m = getWord8 >>= \a -> case a of
    0 -> Unhinted <$> m
    1 -> Hinted <$> deserialize <*> m
    _ -> fail "Hinted.deserialize: unexpected case"

instance Serial a => Serial (Hinted a) where
  serialize = serialize1
  deserialize = deserialize1

instance Binary a => Binary (Hinted a) where
  put = serializeWith Binary.put ; get = deserializeWith Binary.get

instance Serialize a => Serialize (Hinted a) where
  put = serializeWith Serialize.put ; get = deserializeWith Serialize.get

instance Hashable a => Hashable (Hinted a) where
  hashWithSalt s = hashWithSalt s . extract
instance Hashable1 Hinted

instance Digestable a => Digestable (Hinted a) where
  digest c = digest c . extract

instance Digestable1 Hinted
