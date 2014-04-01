{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE TemplateHaskell #-}
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
  , _SigP
  , _WildcardP
  , _AsP
  , _StrictP
  , _LazyP
  , _LitP
  , _ConP
  , _TupP
  , _ConP'
  , _LitP'
  , PatternHead(..)
  , _TupH
  , _ConH
  , arity
  , unboxedArity
  , headName
  , traverseHead
  , PatternPath(..)
  , PatternPaths
  , fieldPP
  , unboxedFieldPP
  , argPP
  , leafPP
  , paths
  , manyPaths
  , Guarded(..)
  , Alt(..)
  , bitraverseAlt
  , patternHead
  , prune
  , forces
  , irrefutable
  -- , getAlt, putAlt, getPat, putPat
  , serializeAlt3
  , deserializeAlt3
  ) where

import Bound
import Control.Applicative
import Control.Monad
import Control.Lens
import Data.Bitraversable
import qualified Data.Binary as Binary
import Data.Binary (Binary)
import Data.Bytes.Serial
import Data.Bytes.Get
import Data.Bytes.Put
import Data.Foldable
import Data.Hashable
import Data.Monoid
import qualified Data.Serialize as Serialize
import Data.Serialize (Serialize)
import Data.Word
import Ermine.Syntax
import Ermine.Syntax.Global
import Ermine.Syntax.Literal
import Ermine.Syntax.Scope
import GHC.Generics

-- | Patterns used by 'Term'
data Pattern t
  = SigP t
  | WildcardP
  | AsP (Pattern t)
  | StrictP (Pattern t)
  | LazyP (Pattern t)
  | LitP !Literal
  | ConP {-# UNPACK #-} !Word8 !Global [Pattern t] -- # of unboxed arguments, the constructor name, and patterns
  | TupP [Pattern t]
  deriving (Eq, Show, Functor, Foldable, Traversable)

makePrisms ''Pattern

_ConP' :: Word8 -> Global -> Prism' (Pattern t) [Pattern t]
_ConP' u g = prism (ConP u g) $ \ xs -> case xs of
  ConP u' g' ps | u == u' && g == g' -> Right ps
  p -> Left p

_LitP' :: Literal -> Prism' (Pattern t) [Pattern t]
_LitP' l = prism (\[] -> LitP l) $ \xs -> case xs of
  LitP l' | l == l' -> Right []
  p -> Left p

-- | Paths into a pattern tree. These will be used as the bound variables
-- for scopes that have patterns in their binder. No effort has been made
-- to enforce statically that pattern paths used in a given scope
-- correspond only to well-formed paths into the particular tree; it will
-- merely have to be an invariant maintained in the compiler.
--
-- LeafPP represents a reference to the variable at a node that binds
-- a variable. For instance, LeafPP is a valid path into SigP. It is also
-- valid for AsP p for all p, because AsP binds a variable. It is invalid
-- for ConP, however.
--
-- No effort is made to represent paths with no decision points. This is
-- primarily because it is detrimental to the compilation strategy. AsP,
-- StrictP and LazyP can be thought of as annotations that are not
-- full-fledged patterns in themselves. AsP signals that it is valid to
-- make reference to an entire piece of substructure, while StrictP and
-- LazyP inform the pattern compilation process. But it is unnecessary to
-- distinguish between pointing at an AsP---which is always valid---and
-- pointing at the right portion of an AsP---which is invalid unless the
-- pattern contains redundancies.
data PatternPath
  = LeafPP -- ^ refer to a variable
  | FieldPP {-# UNPACK #-} !Word8 PatternPath -- ^ refer to the n-th subpattern of a constructor
  | UnboxedFieldPP {-# UNPACK #-} !Word8 PatternPath
  | ArgPP {-# UNPACK #-} !Word8 PatternPath -- ^ refer to the n-th pattern of many top-level patterns
  deriving (Eq, Ord, Show, Read, Generic)

type PatternPaths = Endo PatternPath

unboxedFieldPP :: Word8 -> PatternPaths
unboxedFieldPP = Endo . UnboxedFieldPP

fieldPP :: Word8 -> PatternPaths
fieldPP = Endo . FieldPP

argPP :: Word8 -> PatternPaths
argPP = Endo . ArgPP

leafPP :: PatternPaths -> PatternPath
leafPP = flip appEndo LeafPP

-- | Returns all the paths pointing to variables in the given pattern, in
-- order from left to right.
paths :: Pattern t -> [PatternPath]
paths = go mempty
 where
 go pp (SigP    _)   = [leafPP pp]
 go pp (AsP     p)   = leafPP pp : go pp p
 go pp (StrictP p)   = go pp p
 go pp (LazyP   p)   = go pp p
 go pp (ConP u _ ps) = join $ zipWith (\i -> go $ pp <> (if i < u then unboxedFieldPP i else fieldPP i)) [0..] ps
 go pp (TupP   ps)   = join $ zipWith (\i -> go $ pp <> fieldPP i) [0..] ps
 go _  _             = []

-- | Returns all the paths pointing to variables in a series of patterns.
-- The list is assumed to contain patterns in left-to-right order, and the
-- resulting list will be in the same order.
manyPaths :: [Pattern t] -> [PatternPath]
manyPaths = join . zipWith (\i -> map (ArgPP i) . paths) [0..]

instance Hashable PatternPath

data PatternHead
  = TupH
    { _arity :: {-# UNPACK #-} !Word8
    }
  | ConH
    { _arity, _unboxedArity :: {-# UNPACK #-} !Word8
    , _name :: Global
    }
  | LitH !Literal
  deriving (Eq, Ord, Show)

makePrisms ''PatternHead

arity :: PatternHead -> Word8
arity (TupH n) = n
arity (ConH n _ _) = n
arity LitH{} = 0

unboxedArity :: PatternHead -> Word8
unboxedArity TupH{} = 0
unboxedArity (ConH _ u _) = u
unboxedArity LitH{} = 0

headName :: Traversal' PatternHead Global
headName f (ConH a u g) = ConH a u <$> f g
headName _ h            = pure h

patternHead :: Fold (Pattern t) PatternHead
patternHead f p@(ConP u g ps) = p <$ f (ConH (fromIntegral $ length ps) u g)
patternHead f p@(TupP ps)   = p <$ f (TupH . fromIntegral $ length ps)
patternHead f (AsP p)       = patternHead f p
patternHead f (StrictP p)   = patternHead f p
patternHead f p@(LitP l)    = p <$ f (LitH l)
patternHead _ p             = pure p

traverseHead :: PatternHead -> Traversal' (Pattern t) [Pattern t]
traverseHead (ConH _ u g)    = _ConP' u g
traverseHead (TupH _)        = _TupP
traverseHead (LitH l)        = _LitP' l

prune :: Pattern t -> Pattern t
prune (AsP p)     = prune p
prune (StrictP p) = prune p
prune p           = p

forces :: Pattern t -> Bool
forces ConP{}    = True
forces TupP{}    = True
forces StrictP{} = True
forces LitP{}    = True
forces (AsP p)   = forces p
forces _         = False

-- | A datatype for representing potentially guarded cases of a function
-- or case body.
data Guarded tm
  = Unguarded tm
  | Guarded [(tm, tm)]
  deriving (Eq, Ord, Show, Read, Functor, Foldable, Traversable, Generic)

-- | One alternative of a core expression
data Alt t f a = Alt !(Pattern t) (Guarded (Scope PatternPath f a))
  deriving (Eq,Show,Functor,Foldable,Traversable)

instance Bound (Alt t) where
  Alt p b >>>= f = Alt p (fmap (>>>= f) b)

instance Monad f => BoundBy (Alt t f) f where
  boundBy f (Alt p b) = Alt p (fmap (boundBy f) b)

-- | Helper function for traversing both sides of an 'Alt'.
bitraverseAlt :: (Bitraversable k, Applicative f) => (t -> f t') -> (a -> f b) -> Alt t (k t) a -> f (Alt t' (k t') b)
bitraverseAlt f g (Alt p b) =
  Alt <$> traverse f p <*> traverse (bitraverseScope f g) b

irrefutable :: Pattern t -> Bool
irrefutable (SigP _)  = True
irrefutable WildcardP = True
irrefutable (LazyP _) = True
irrefutable (AsP p)   = irrefutable p
irrefutable _         = False

instance (Bifunctor p, Choice p, Applicative f) => Tup p f (Pattern t) where
  _Tup = prism TupP $ \p -> case p of
    TupP ps -> Right ps
    _       -> Left p
  {-# INLINE _Tup #-}

instance (Serial t, Serial1 f) => Serial1 (Alt t f) where
  serializeWith pa (Alt p s) = do
    serialize p
    serializeWith (serializeWith pa) s

  deserializeWith ga = liftM2 Alt deserialize (deserializeWith $ deserializeWith ga)

instance Serial1 Guarded where
  serializeWith ptm (Unguarded tm) = putWord8 0 *> ptm tm
  serializeWith ptm (Guarded tms)  = putWord8 1 *> serializeWith putPair tms where
    putPair (tm1, tm2) = ptm tm1 *> ptm tm2

  deserializeWith gtm = getWord8 >>= \b -> case b of
    0 -> Unguarded <$> gtm
    1 -> Guarded <$> deserializeWith ((,) <$> gtm <*> gtm)
    _ -> fail $ "getGuarded: Unexpected constructor code: " ++ show b

instance Serial v => Serial (Guarded v) where
  serialize = serialize1 ; deserialize = deserialize1

instance Binary tm => Binary (Guarded tm) where
  put = serializeWith   Binary.put
  get = deserializeWith Binary.get

instance Serialize tm => Serialize (Guarded tm) where
  put = serializeWith Serialize.put
  get = deserializeWith Serialize.get

instance (Serial t, Serial1 f, Serial a) => Serial (Alt t f a) where
  serialize = serializeWith serialize
  deserialize = deserializeWith deserialize

serializeAlt3
  :: MonadPut m
  => (t -> m ()) -> (forall a. (a -> m ()) -> f a -> m ()) -> (v -> m ())
  -> Alt t f v -> m ()
serializeAlt3 pt pf pv (Alt p b) = serializeWith pt p *> serializeWith (serializeScope3 serialize pf pv) b

deserializeAlt3
  :: MonadGet m
  => m t -> (forall a. m a -> m (f a)) -> m v -> m (Alt t f v)
deserializeAlt3 gt gf gv =
  Alt <$> deserializeWith gt <*> deserializeWith (deserializeScope3 deserialize gf gv)

instance Serial1 Pattern where
  serializeWith pt (SigP t)      = putWord8 0 >> pt t
  serializeWith _  WildcardP     = putWord8 1
  serializeWith pt (AsP p)       = putWord8 2 >> serializeWith pt p
  serializeWith pt (StrictP p)   = putWord8 3 >> serializeWith pt p
  serializeWith pt (LazyP p)     = putWord8 4 >> serializeWith pt p
  serializeWith _  (LitP l)      = putWord8 5 >> serialize l
  serializeWith pt (ConP u g ps) = putWord8 6 >> serialize u >> serialize g >> serializeWith (serializeWith pt) ps
  serializeWith pt (TupP ps)     = putWord8 7 >> serializeWith (serializeWith pt) ps

  deserializeWith gt = getWord8 >>= \b -> case b of
    0 -> liftM SigP gt
    1 -> return WildcardP
    2 -> liftM AsP $ deserializeWith gt
    3 -> liftM StrictP $ deserializeWith gt
    4 -> liftM LazyP $ deserializeWith gt
    5 -> liftM LitP deserialize
    6 -> liftM3 ConP deserialize deserialize (deserializeWith $ deserializeWith gt)
    7 -> liftM TupP $ deserializeWith (deserializeWith gt)
    _ -> fail $ "get Pattern: unexpected constructor tag: " ++ show b

instance Serial a => Serial (Pattern a) where
  serialize = serializeWith serialize
  deserialize = deserializeWith deserialize

instance Binary t => Binary (Pattern t) where
  put = serializeWith Binary.put
  get = deserializeWith Binary.get

instance Serialize t => Serialize (Pattern t) where
  put = serializeWith Serialize.put
  get = deserializeWith Serialize.get

instance Serial PatternPath where
instance Binary PatternPath where put = serialize ; get = deserialize
instance Serialize PatternPath where put = serialize ; get = deserialize
