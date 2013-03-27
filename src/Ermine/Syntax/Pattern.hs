{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Ermine.Syntax.Pattern
  ( Pattern(..)
  , Alt(..)
  , bitraverseAlt
  , getAlt, putAlt, getPat, putPat
  ) where

import Bound
import Control.Applicative
import Control.Lens
import Control.Monad
import Data.Bitraversable
import Data.Binary as Binary
import Data.Foldable
import Ermine.Syntax
import Ermine.Syntax.Global
import Ermine.Syntax.Literal
import Ermine.Syntax.Scope

-- | Patterns used by 'Term'
data Pattern t
  = SigP t
  | WildcardP
  | AsP (Pattern t)
  | StrictP (Pattern t)
  | LazyP (Pattern t)
  | LitP Literal
  | ConP Global [Pattern t]
  | TupP [Pattern t]
  deriving (Eq, Show, Functor, Foldable, Traversable)

-- | One alternative of a core expression
data Alt t f a = Alt !(Pattern t) !(Scope Int f a)
  deriving (Eq,Show,Functor,Foldable,Traversable)

instance Bound (Alt t) where
  Alt p b >>>= f = Alt p (b >>>= f)

instance Monad f => BoundBy (Alt t f) f where
  boundBy f (Alt p b) = Alt p (boundBy f b)

-- | Helper function for traversing both sides of an 'Alt'.
bitraverseAlt :: (Bitraversable k, Applicative f) => (t -> f t') -> (a -> f b) -> Alt t (k t) a -> f (Alt t' (k t') b)
bitraverseAlt f g (Alt p b) = Alt <$> traverse f p <*> bitraverseScope f g b

instance (Bifunctor p, Choice p, Applicative f) => Tup p f (Pattern t) where
  tupled = prism TupP $ \p -> case p of TupP ps -> Right ps ; _ -> Left p

putMany :: (k -> Put) -> [k] -> Put
putMany p ls = put (length ls) *> traverse_ p ls

getMany :: Get k -> Get [k]
getMany g = get >>= \n -> replicateM n g

-- | Binary serialization of a 'Alt', given serializers for its parameters.
putAlt :: (t -> Put) -> (forall x. (x -> Put) -> f x -> Put) -> (a -> Put) -> Alt t f a -> Put
putAlt pt pf pa (Alt p s) = putPat pt p *> putScope put pf pa s

getAlt :: Get t -> (forall x. Get x -> Get (f x)) -> Get a -> Get (Alt t f a)
getAlt gt gf ga = Alt <$> getPat gt <*> getScope get gf ga

-- | Binary serialization of a 'Pattern', given serializers for its parameter.
putPat :: (t -> Put) -> Pattern t -> Put
putPat pt (SigP t)    = putWord8 0 *> pt t
putPat _  WildcardP   = putWord8 1
putPat pt (AsP p)     = putWord8 2 *> putPat pt p
putPat pt (StrictP p) = putWord8 3 *> putPat pt p
putPat pt (LazyP p)   = putWord8 4 *> putPat pt p
putPat _  (LitP l)    = putWord8 5 *> put l
putPat pt (ConP g ps) = putWord8 6 *> put g *> putMany (putPat pt) ps
putPat pt (TupP ps)   = putWord8 7 *> putMany (putPat pt) ps

getPat :: Get t -> Get (Pattern t)
getPat gt = getWord8 >>= \b -> case b of
  0 -> SigP <$> gt
  1 -> return WildcardP
  2 -> AsP     <$> getPat gt
  3 -> StrictP <$> getPat gt
  4 -> LazyP   <$> getPat gt
  5 -> LitP    <$> get
  6 -> ConP    <$> get <*> getMany (getPat gt)
  7 -> TupP    <$> getMany (getPat gt)
  _ -> fail $ "get Pattern: unexpected constructor tag: " ++ show b

instance Binary t => Binary (Pattern t) where
  put = putPat put
  get = getPat get
