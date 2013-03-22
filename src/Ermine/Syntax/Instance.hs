{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
module Ermine.Syntax.Instance
  ( Head(..)
  , HasHead(..)
  ) where

import Control.Lens
import Data.Data
import Data.Hashable
import Ermine.Syntax.Core
import Ermine.Syntax.Kind
import Ermine.Syntax.Type
import Ermine.Syntax.Global
import GHC.Generics

data Head = Head
  { _headClass     :: !Global
  , _headKindVars  :: !Int
  , _headTypeKinds :: [Kind Int]
  , _headKinds     :: [Kind Int]
  , _headTypes     :: [Type Int Int]
  } deriving (Show,Eq,Generic,Typeable)

makeLensesWith ?? ''Head $ classyRules & lensClass .~ \_ -> Just ("HasHead","head_")

instance Hashable Head

data Instance = Instance
  { _instanceContext :: [Type Int Int]
  , _instanceHead :: Head
  , _instanceBody :: Core Id
  }

data Id
  = GlobalId Global
  | InstanceId Head

-- instance Ord a => Ord [a]
-- Head ord 0 [star] [] [list (pure 0)]

-- instance Category (:-)
-- Head category 0 [] [constraint] [con ":-" ...]
