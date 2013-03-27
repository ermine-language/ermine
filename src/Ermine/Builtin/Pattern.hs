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
  , P
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
import Control.Lens.Internal.Review
import Data.List as List
import Data.Foldable
import Data.Functor.Identity
import Data.Traversable
import Ermine.Syntax
import Ermine.Syntax.Global
import Ermine.Syntax.Literal
import Ermine.Syntax.Pattern

data Binder v a = Binder { vars :: [v], item :: a }
  deriving (Eq, Ord, Show, Read, Functor, Foldable, Traversable)

instance Applicative (Binder v) where
  pure = Binder []
  Binder ls f <*> Binder rs x = Binder (ls ++ rs) (f x)

instance Comonad (Binder v) where
  extract = item
  extend f b = b { item = f b }

instance (p ~ Reviewed, f ~ Identity, Tup Reviewed Identity t) => Tup p f (Binder v t) where
  tupled = unto (fmap tup . sequenceA)

-- | Smart pattern
type P t v = Binder v (Pattern t)

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

-- | A pattern that matches a constructor expression.
conp :: Global -> [P t v] -> P t v
conp g ps = ConP g <$> sequenceA ps

-- | A pattern that matches a literal value
litp :: Literal -> P t v
litp = pure . LitP

-- | smart alt constructor
alt :: (Monad f, Eq v) => P t v -> f v -> Alt t f v
alt (Binder vs p) = Alt p . abstract (`List.elemIndex` vs)
