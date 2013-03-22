{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
module Ermine.Syntax.Head where

import Control.Lens
import Data.Data
import Data.Hashable
import Ermine.Syntax.Kind
import Ermine.Syntax.Type
import Ermine.Syntax.Global

data Head = Head
  { _headClass     :: !Global
  , _headKindVars  :: !Int
  , _headTypeKinds :: [Kind Int]
  , _headKinds     :: [Kind Int]
  , _headTypes     :: [Type Int Int]
  } deriving (Show,Eq) -- ,Data,Typeable)

makeLensesWith ?? ''Head $ classyRules & lensClass .~ \x -> Just ("HasHead","head_")

-- instance Hashable Head where

-- instance Ord a => Ord [a]
-- Head ord 0 [star] [] [list (pure 0)]

-- instance Category (:-)
-- Head category 0 [] [constraint] [con ":-" ...]
