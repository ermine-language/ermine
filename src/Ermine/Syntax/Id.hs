{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett and Dan Doel 2013
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Ermine.Syntax.Id
  ( Id(..)
  ) where

import Control.Lens
import Data.Hashable
import Data.Typeable
import Ermine.Syntax.Head
import Ermine.Syntax.Global
import GHC.Generics

------------------------------------------------------------------------------
-- Id
------------------------------------------------------------------------------

data Id
  = GlobalId !Global
  | InstanceId !Head
  deriving (Show,Eq,Typeable,Generic)

makePrisms ''Id

instance AsGlobal Id where
  _Global = _GlobalId

instance AsHead Id where
  _Head = _InstanceId

instance Hashable Id
