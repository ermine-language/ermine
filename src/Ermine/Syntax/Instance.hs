{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
module Ermine.Syntax.Instance
  ( Head(..)
  , HasHead(..)
  , Instance(..)
  , HasInstance(..)
  , Id(..)
  ) where

import Control.Lens
import Data.Data
import Data.Hashable
import Ermine.Syntax.Core
import Ermine.Syntax.Kind
import Ermine.Syntax.Type
import Ermine.Syntax.Global
import GHC.Generics

------------------------------------------------------------------------------
-- Head
------------------------------------------------------------------------------

data Head = Head !Int !Global !Int [Kind Int] [Kind Int] [Type Int Int]
  deriving (Show,Eq,Generic,Typeable)

class AsHead t where
  _Head :: Prism' t Head

instance AsHead Head where
  _Head = id
  {-# INLINE _Head #-}

mkHead :: Global -> Int -> [Kind Int] -> [Kind Int] -> [Type Int Int] -> Head
mkHead g i tks ks ts = Head (hash g `hashWithSalt` i `hashWithSalt` tks `hashWithSalt` ks `hashWithSalt` ts) g i tks ks ts
{-# INLINE mkHead #-}

class HasHead t where
  head_         :: Lens' t Head

  headClass     :: Lens' t Global
  headClass f = head_ $ \(Head _ g i tks ks ts) -> f g <&> \g' -> mkHead g' i tks ks ts

  headKindVars  :: Lens' t Int
  headKindVars f = head_ $ \(Head _ g i tks ks ts) -> f i <&> \i' -> mkHead g i' tks ks ts

  headTypeKinds :: Lens' t [Kind Int]
  headTypeKinds f = head_ $ \(Head _ g i tks ks ts) -> f tks <&> \tks' -> mkHead g i tks' ks ts

  headKinds :: Lens' t [Kind Int]
  headKinds f = head_ $ \(Head _ g i tks ks ts) -> f ks <&> \ks' -> mkHead g i tks ks' ts

  headTypes :: Lens' t [Type Int Int]
  headTypes f = head_ $ \(Head _ g i tks ks ts) -> f ts <&> \ts' -> mkHead g i tks ks ts'

instance HasHead Head where
  head_ = id

instance Hashable Head where
  hashWithSalt n (Head h _ _ _ _ _) = hashWithSalt n h

------------------------------------------------------------------------------
-- Id
------------------------------------------------------------------------------

data Id
  = GlobalId !Global
  | InstanceId !Head

makePrisms ''Id

instance AsGlobal Id where
  _Global = _GlobalId

instance AsHead Id where
  _Head = _InstanceId

------------------------------------------------------------------------------
-- Instance
------------------------------------------------------------------------------

-- instance Ord a => Ord [a]
-- Instance [ord (pure 0)] (Head ord 0 [star] [] [list (pure 0)]) (LamDict o (Scope (Dict [AppDict (InstanceId (Head eq 0 [star] [] [list (pure 0))) (AppDict (Slot 0) (B ()))] ...)))

-- instance Category (:-)
-- Instnace [] (Head category 0 [] [constraint] [con ":-" ...]) (Dict ... ...)

data Instance = Instance
  { _instanceContext :: [Type Int Int]
  , _instanceHead :: Head
  , _instanceBody :: Core Id
  }

makeLensesWith ?? ''Instance $ classyRules & lensClass .~ \_ -> Just ("HasInstance","instance_")

instance HasHead Instance where
  head_ = instanceHead

