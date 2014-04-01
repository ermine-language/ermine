{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett, Dan Doel 2012-2013
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
-- Smart builders for convenient building of patterns.
--------------------------------------------------------------------
module Ermine.Builtin.Pattern
  ( Binder(..)
  , note
  , noted
  , P
  , varp
  , sigp
  , _p
  , strictp
  , lazyp
  , asp
  , conp
  , litp
  , alt
  ) where

import Bound
import Control.Applicative
import Control.Comonad
import Control.Lens
import Data.Bifoldable
import Data.Bitraversable
import Data.Foldable
import Data.Monoid
import Data.String
import Data.Tagged
import Data.Traversable
import Data.Word
import Ermine.Syntax
import Ermine.Syntax.Global
import Ermine.Syntax.Kind
import Ermine.Syntax.Literal
import Ermine.Syntax.Pattern
import Ermine.Syntax.Type

data Binder v a = Binder { vars :: [v], item :: a }
  deriving (Eq, Ord, Show, Read, Functor, Foldable, Traversable)

instance Bifunctor Binder where
  bimap f g (Binder vs a) = Binder (map f vs) (g a)

instance Bifoldable Binder where
  bifoldMap f g (Binder vs a) = foldMap f vs <> g a

instance Bitraversable Binder where
  bitraverse f g (Binder vs a) = Binder <$> traverse f vs <*> g a

-- | If the annotation and item types correspond, we can push the item
-- onto the annotations
note :: Binder v v -> Binder v v
note (Binder vs v) = Binder (v:vs) v

-- | Injects a value into a binder, placing the value in the annotations as
-- well.
noted :: v -> Binder v v
noted = note . pure

instance Applicative (Binder v) where
  pure = Binder []
  Binder ls f <*> Binder rs x = Binder (ls ++ rs) (f x)

instance Comonad (Binder v) where
  extract = item
  extend f b = b { item = f b }

instance (p ~ Tagged, f ~ Identity, Tup Tagged Identity t) => Tup p f (Binder v t) where
  _Tup = unto (fmap tup . sequenceA)

-- | Smart pattern
type P t v = Binder v (Pattern t)

instance IsString s => IsString (P (Annot k t) s) where
  fromString = varp . fromString

-- | A pattern that binds a variable with (effectively) no type annotation.
varp :: v -> P (Annot k t) v
varp v = sigp v (Annot [star] . Scope . pure . B $ 0)

-- | A pattern that binds a variable with a type annotation.
sigp :: v -> t -> P t v
sigp v t = Binder [v] $ SigP t

-- | A wildcard pattern that ignores its argument
_p :: P t v
_p = pure WildcardP

-- | A strict (bang) pattern
strictp :: P t v -> P t v
strictp = fmap StrictP

-- | A lazy (irrefutable) pattern
lazyp :: P t v -> P t v
lazyp = fmap LazyP

-- | An as @(\@)@ pattern.
asp :: v -> P t v -> P t v
asp v (Binder vs p) = Binder (v:vs) $ AsP p

-- | A pattern that matches a constructor expression, with a specified number of unboxed arguments.
conp :: Word8 -> Global -> [P t v] -> P t v
conp u g ps = ConP u g <$> sequenceA ps

-- | A pattern that matches a literal value
litp :: Literal -> P t v
litp = pure . LitP

-- | smart alt constructor
alt :: (Monad f, Eq v) => P t v -> Guarded (f v) -> Alt t f v
alt (Binder vs p) = Alt p . fmap (abstract (`lookup` zip vs (paths p)))
