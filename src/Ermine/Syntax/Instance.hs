{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett and Dan Doel 2012-2013
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Ermine.Syntax.Instance
  ( Instance(..)
  , HasInstance(..)
  ) where

import Control.Lens
import Data.Hashable
import Data.Typeable
import Data.Void
import Ermine.Syntax.Core
import Ermine.Syntax.Head
import Ermine.Syntax.Id
import Ermine.Syntax.Type
import GHC.Generics

------------------------------------------------------------------------------
-- Instance
------------------------------------------------------------------------------

-- instance Ord a => Ord [a]
-- Instance [ord (pure 0)] (Head ord 0 [star] [] [list (pure 0)]) (LamDict (Scope (Dict [AppDict (InstanceId (Head eq 0 [star] [] [list (pure 0))) (AppDict (Slot 0) (B ()))] ...)))

-- instance Category (:-)
-- Instance [] (Head category 0 [] [constraint] [con ":-" ...]) (Dict [] ...)

data Instance = Instance
  { _instanceContext :: [Type Void Int]
  , _instanceHead :: Head
  , _instanceBody :: Core Id
  } deriving (Eq, Typeable, Generic, Show)

makeLensesWith ?? ''Instance $ classyRules & lensClass .~ \_ -> Just ("HasInstance","instance_")

instance HasHead Instance where
  head_ = instanceHead

instance Hashable Instance
