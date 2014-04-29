{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett and Dan Doel 2013-2014
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Ermine.Syntax.Id
  ( Id(..)
  , AsId(..)
  ) where

import Control.Applicative
import Control.Lens
import qualified Data.Binary as Binary
import Data.Binary (Binary)
import Data.Bytes.Get
import Data.Bytes.Put
import Data.Bytes.Serial
import Data.Data
import Data.Hashable
import Data.Serialize (Serialize)
import qualified Data.Serialize as Serialize
import Ermine.Syntax.Head
import Ermine.Syntax.Global
import GHC.Generics


------------------------------------------------------------------------------
-- Id
------------------------------------------------------------------------------

data Id
  = GlobalId !Global
  | InstanceId !Head
  deriving (Show,Read,Eq,Ord,Typeable,Data,Generic)

-- _Global = _GlobalId
class AsGlobal t => AsId t where
  _Id :: Prism' t Id
  _InstanceId :: Prism' t Head
  _InstanceId = _Id._InstanceId

instance AsGlobal Id where
  _Global = prism GlobalId $ \xs -> case xs of
    GlobalId x -> Right x
    _          -> Left xs

instance AsId Id where
  _Id = id
  _InstanceId = prism InstanceId $ \xs -> case xs of
    InstanceId x -> Right x
    _           -> Left xs

instance Hashable Id

instance Serial Id where
  serialize (GlobalId g)    = putWord8 0 >> serialize g
  serialize (InstanceId h)  = putWord8 1 >> serialize h

  deserialize = getWord8 >>= \b -> case b of
    0 -> GlobalId <$> deserialize
    1 -> InstanceId <$> deserialize
    _ -> fail $ "get Id: Unexpected constructor code: " ++ show b

instance Binary Id where
  get = deserialize
  put = serialize

instance Serialize Id where
  get = deserialize
  put = serialize
