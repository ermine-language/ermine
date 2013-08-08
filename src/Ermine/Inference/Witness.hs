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
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Ermine.Inference.Witness
  ( Witness(Witness)
  , HasWitness(..)
  ) where

import Bound
import Bound.Var
import Control.Applicative
import Control.Lens
import Data.Bifunctor
import Data.Bifoldable
import Data.Bitraversable
import Data.Foldable
import Data.Typeable
import Ermine.Syntax.Core
import Ermine.Syntax.Kind
import Ermine.Syntax.Type
import GHC.Generics
import Prelude.Extras

data Witness t a = Witness
  { _witnessRowConstraints :: [t]
  , _witnessType :: !t
  , _witnessCore :: !(Core (Var a t))
  } deriving (Show, Eq, Typeable, Generic)

makeClassy ''Witness

witnessTypes :: Traversal (Witness t a) (Witness t' a) t t'
witnessTypes f (Witness rcs t c) = Witness <$> traverse f rcs <*> f t <*> traverse (traverse f) c

instance HasTypeVars t t' v v' => HasTypeVars (Witness t a) (Witness t' a) v v' where
  typeVars = witnessTypes.typeVars

instance HasKindVars t t' v v' => HasKindVars (Witness t a) (Witness t' a) v v' where
  kindVars = witnessTypes.kindVars

instance Eq2 Witness
instance Eq k => Eq1 (Witness k)

instance Show2 Witness
instance Show k => Show1 (Witness k)

instance Functor (Witness t) where
  fmap f (Witness ts t cs) = Witness ts t (fmap (first f) cs)

instance Foldable (Witness t) where
  foldMap f (Witness _ _ cs) = foldMap (foldMapOf _B f) cs

instance Traversable (Witness t) where
  traverse f (Witness t ts cs) = Witness t ts <$> traverse (_B f) cs

instance Bifunctor Witness where
  bimap = bimapDefault

instance Bifoldable Witness where
  bifoldMap = bifoldMapDefault

instance Bitraversable Witness where
  bitraverse f g (Witness ts t cs) = Witness <$> traverse f ts <*> f t <*> traverse (bitraverse g f) cs
