{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett and Dan Doel 2012-2013
-- License   :  BSD2
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Ermine.Inference.Witness
  (
  -- * Witnesses
    Witness(Witness)
  , HasWitness(..)
  ) where

import Bound
import Bound.Scope
import Control.Applicative
import Control.Lens
import Data.Bifoldable
import Data.Bitraversable
import Data.Foldable
import Data.Typeable
import Ermine.Syntax.Core
import Ermine.Syntax.Kind
import Ermine.Syntax.Type
import GHC.Generics
import Prelude.Extras

-- * Witness

data Witness c t a = Witness
  { _witnessRowConstraints :: [t]
  , _witnessType :: !t
  , _witnessCore :: !(Scope a (Core c) t)
  } deriving (Show, Eq, Typeable, Generic)

makeClassy ''Witness

witnessTypes :: Traversal (Witness c t a) (Witness c t' a) t t'
witnessTypes f (Witness rcs t c) = Witness <$> traverse f rcs <*> f t <*> traverse f c

instance HasTypeVars t t' v v' => HasTypeVars (Witness c t a) (Witness c t' a) v v' where
  typeVars = witnessTypes.typeVars

instance HasKindVars t t' v v' => HasKindVars (Witness c t a) (Witness c t' a) v v' where
  kindVars = witnessTypes.kindVars

instance Eq c => Eq2 (Witness c)
instance (Eq c, Eq t) => Eq1 (Witness c t)

instance Show c => Show2 (Witness c)
instance (Show c, Show t) => Show1 (Witness c t)

instance Functor (Witness c t) where
  fmap f (Witness ts t cs) = Witness ts t (mapBound f cs)

instance Foldable (Witness c t) where
  foldMap f (Witness _ _ cs) = foldMapBound f cs

instance Traversable (Witness c t) where
  traverse f (Witness t ts cs) = Witness t ts <$> traverseBound f cs

instance Bifunctor (Witness c) where
  bimap = bimapDefault

instance Bifoldable (Witness c) where
  bifoldMap = bifoldMapDefault

instance Bitraversable (Witness c) where
  bitraverse f g (Witness ts t cs) = Witness <$> traverse f ts <*> f t <*> traverseScope g f cs
