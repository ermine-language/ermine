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

-- | In the instance declaration:
--
--   instance Foo a => Bar Int [a] where
--     ...
--
-- The portion 'Bar Int [a]' is the head of the instance. All instances
-- must have a unique head; type class dispatch does not take the context
-- of the instance into account. Thus, instances are uniquely identified by
-- their heads, and we use them for that purpose among others.
--
-- The Head type records all the information necessary for this unique
-- identification, plus a hash for fast equality checks.
--
-- The above instance head would be represented as something like:
--
--   Head ... "Bar" 0 [star] [] [int, list (B 0)]
data Head = Head { _hash           :: !Int
                 , _headClass      :: !Global    -- ^ The class name for which this is an instance
                 , _headBoundKinds :: !Int       -- ^ number of kind variables brought into scope
                 , _headBoundTypes :: [Kind Int] -- ^ kinds of type variables brought into scope
                 , _headKindArgs   :: [Kind Int]
                 , _headTypeArgs   :: [Type Int Int]
                 }
  deriving (Show,Eq,Generic,Typeable)

class AsHead t where
  _Head :: Prism' t Head

instance AsHead Head where
  _Head = id
  {-# INLINE _Head #-}

instance HasGlobal Head where
  global f = head_ $ \(Head _ g i tks ks ts) -> f g <&> \g' -> mkHead g' i tks ks ts

mkHead :: Global -> Int -> [Kind Int] -> [Kind Int] -> [Type Int Int] -> Head
mkHead g i tks ks ts = Head (hash g `hashWithSalt` i `hashWithSalt` tks `hashWithSalt` ks `hashWithSalt` ts) g i tks ks ts
{-# INLINE mkHead #-}

class HasHead t where
  head_         :: Lens' t Head

  headClass     :: Lens' t Global
  headClass f = head_ $ \(Head _ g i tks ks ts) -> f g <&> \g' -> mkHead g' i tks ks ts

  headBoundKinds :: Lens' t Int
  headBoundKinds f = head_ $ \(Head _ g i tks ks ts) -> f i <&> \i' -> mkHead g i' tks ks ts

  headBoundTypes :: Lens' t [Kind Int]
  headBoundTypes f = head_ $ \(Head _ g i tks ks ts) -> f tks <&> \tks' -> mkHead g i tks' ks ts

  headKindArgs :: Lens' t [Kind Int]
  headKindArgs f = head_ $ \(Head _ g i tks ks ts) -> f ks <&> \ks' -> mkHead g i tks ks' ts

  headTypeArgs :: Lens' t [Type Int Int]
  headTypeArgs f = head_ $ \(Head _ g i tks ks ts) -> f ts <&> \ts' -> mkHead g i tks ks ts'

instance HasHead Head where
  head_ = id

instance Hashable Head where
  hashWithSalt n (Head h _ _ _ _ _) = hashWithSalt n h
