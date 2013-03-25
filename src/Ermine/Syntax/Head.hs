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
module Ermine.Syntax.Head
  ( Head(..)
  , HasHead(..)
  , AsHead(..)
  ) where

import Control.Lens
import Data.Data
import Data.Hashable
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
