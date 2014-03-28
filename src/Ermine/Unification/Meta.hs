{-# LANGUAGE CPP #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

#ifndef MIN_VERSION_base
#define MIN_VERSION_base(x,y,z) 0
#endif
--------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett 2011-2012
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
-- Skolem and Meta variables
--------------------------------------------------------------------
module Ermine.Unification.Meta
  (
  -- * Meta variables
    Meta(Meta,Skolem)
  , metaId, metaValue, metaRef
  -- * Meta Types
  , MetaT, TypeM
  -- * Meta Kinds
  , MetaK, KindM
  -- ** Meta Prisms
  , _Skolem
  -- ** λ-Depth
  , Depth, metaDepth, depthInf
  -- ** Union-By-Rank
  , Rank, metaRank, bumpRank
  -- ** Working with Meta
  , newMeta, newShallowMeta
  , newSkolem, newShallowSkolem
  , readMeta
  , writeMeta
  -- ** Pruning
  , semiprune
  , zonk
  , zonk_
  , zonkWith
  , zonkScope
  , zonkScope_
  -- * MetaEnv
  , MetaEnv
  , HasMetaEnv(..)
  , MonadMeta(..)
  , viewMeta
  -- * The unification monad
  , M, runM, runM_, ioM
  , throwM
  , fresh
  , remember
  ) where

import Bound
import Control.Applicative
import Control.Exception
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Control.Monad.ST (ST, runST, stToIO)
import Control.Monad.ST.Class
import Control.Monad.ST.Unsafe
import Control.Monad.Trans
import Control.Monad.State.Lazy as Lazy
import Control.Monad.State.Strict as Strict
import Control.Monad.Writer.Class
import Data.Function (on)
import Data.IntMap
import Data.Monoid
import Data.STRef
import Data.Text (pack)
import Data.Traversable
import Data.Word
import Ermine.Diagnostic
import Ermine.Syntax
import Ermine.Syntax.Type
import Ermine.Syntax.Kind
import Ermine.Unification.Sharing

------------------------------------------------------------------------------
-- Meta
------------------------------------------------------------------------------

-- | Kuan and MacQueen's notion of λ-ranking.
type Depth = Int

-- | This plays the role of infinity.
depthInf :: Depth
depthInf = maxBound

-- | Rank for union-by-rank viewing meta-variables as disjoint set forests
type Rank = Word

bumpRank :: STRef s Rank -> ST s ()
#if MIN_VERSION_base(4,6,0)
bumpRank w = modifySTRef' w (+1)
#else
bumpRank w = do
  n <- readSTRef w
  let n' = n + 1
  n' `seq` writeSTRef w n'
#endif

-- | A meta variable for skolemization and unification
data Meta s f a
  = Meta   { _metaValue :: a
           , _metaId :: !Int
           , _metaRef :: !(STRef s (Maybe (f (Meta s f a))))
           , _metaDepth :: !(STRef s Depth)
           , _metaRank :: !(STRef s Rank)
           }
  | Skolem { _metaValue :: a
           , _metaId :: !Int
           , _metaDepth :: !(STRef s Depth)
           }

makeLenses ''Meta

------------------------------------------------------------------------------
-- MetaEnv
------------------------------------------------------------------------------

-- | A type meta-variable
type MetaT s = Meta s (Type (MetaK s)) (KindM s)

-- | A type filled with meta-variables
type TypeM s = Type (MetaK s) (MetaT s)

-- | A kind meta-variable
type MetaK s = Meta s Kind ()

-- | A kind filled with meta-variables
type KindM s = Kind (MetaK s)

data MetaEnv s = MetaEnv
  { _metaRendering :: Rendering
  , _metaFresh :: {-# UNPACK #-} !(STRef s Int)
  , _metaMemory :: STRef s (IntMap (TypeM s))
  }

makeClassy ''MetaEnv

instance HasRendering (MetaEnv s) where
  rendering = metaRendering

------------------------------------------------------------------------------
-- More Meta
------------------------------------------------------------------------------

-- | This 'Prism' matches 'Skolem' variables.
_Skolem :: Prism' (Meta s f a) (a, Int, STRef s Depth)
_Skolem = prism (\(a,i,d) -> Skolem a i d) $ \t -> case t of
  Skolem a i d -> Right (a, i, d)
  _ -> Left t

{-
_Meta :: Prism (Meta s f a)
               (Meta s g a)
               (a, Int, STRef s (Maybe (f (Meta s f a))), STRef s Depth, STRef s Rank)
               (a, Int, STRef s (Maybe (g (Meta s g a))), STRef s Depth, STRef s Rank)
_Meta = prism (\(a,i,r,k,u) -> Meta a i r k u) $ \t -> case t of
  Meta a i r k u -> Right (a,i,r,k,u)
  _ -> Left t
-}

instance Show a => Show (Meta s f a) where
  showsPrec d (Meta a i _ _ _) = showParen (d > 10) $
    showString "Meta " . showsPrec 11 a . showChar ' ' . showsPrec 11 i . showString " ..."
  showsPrec d (Skolem a i _) = showParen (d > 10) $
    showString "Skolem " . showsPrec 11 a . showChar ' ' . showsPrec 11 i

instance Eq (Meta s f a) where
  (==) = (==) `on` view metaId
  {-# INLINE (==) #-}

instance Ord (Meta s f a) where
  compare = compare `on` view metaId
  {-# INLINE compare #-}

-- | Construct a new meta variable
newMeta :: MonadMeta s m => a -> m (Meta s f a)
newMeta a = Meta a <$> fresh <*> liftST (newSTRef Nothing) <*> liftST (newSTRef depthInf) <*> liftST (newSTRef 0)
{-# INLINE newMeta #-}

-- | Construct a new meta variable at a given depth
newShallowMeta :: MonadMeta s m => Depth -> a -> m (Meta s f a)
newShallowMeta d a = Meta a <$> fresh <*> liftST (newSTRef Nothing) <*> liftST (newSTRef d) <*> liftST (newSTRef 0)
{-# INLINE newShallowMeta #-}

-- | Construct a new Skolem variable that unifies only with itself.
newSkolem :: MonadMeta s m => a -> m (Meta s f a)
newSkolem a = Skolem a <$> fresh <*> liftST (newSTRef depthInf)
{-# INLINE newSkolem #-}

-- | Construct a new Skolem variable at a given depth
newShallowSkolem :: MonadMeta s m => Depth -> a -> m (Meta s f a)
newShallowSkolem d a = Skolem a <$> fresh <*> liftST (newSTRef d)
{-# INLINE newShallowSkolem #-}

-- | Read a meta variable
readMeta :: MonadMeta s m => Meta s f a -> m (Maybe (f (Meta s f a)))
readMeta (Meta _ _ r _ _) = liftST $ readSTRef r
readMeta Skolem{} = return Nothing
{-# INLINE readMeta #-}

-- | Write to a meta variable
writeMeta :: MonadMeta s m => Meta s f a -> f (Meta s f a) -> m ()
writeMeta (Meta _ _ r _ _) a = liftST $ writeSTRef r (Just a)
writeMeta Skolem{} _   = fail "writeMeta: skolem"
{-# INLINE writeMeta #-}

-- | Path-compression
semiprune :: (Variable f, Monad f, MonadMeta s m) => f (Meta s f a) -> m (f (Meta s f a))
semiprune t0 = case preview _Var t0 of
  Just v0 -> loop t0 v0
  Nothing -> return t0
  where
    loop t1 v1 = readMeta v1 >>= \mb -> case mb of
      Nothing -> return t1
      Just t  -> case preview _Var t of
        Nothing -> return t -- t1?
        Just v  -> do
          fv <- loop t v
          writeMeta v1 fv
          return fv
{-# INLINE semiprune #-}

-- | Expand meta variables recursively
zonk :: (MonadMeta s m, MonadWriter Any m, Traversable f, Monad f) => f (Meta s f a) -> m (f (Meta s f a))
zonk fs = zonkWith fs $ \_ -> return ()
{-# INLINE zonk #-}

zonk_ :: (MonadMeta s m, Traversable f, Monad f) => f (Meta s f a) -> m (f (Meta s f a))
zonk_ fs = runSharing fs $ zonk fs
{-# INLINE zonk_ #-}

zonkWith :: (MonadMeta s m, MonadWriter Any m, Traversable f, Monad f) => f (Meta s f a) -> (Meta s f a -> m ()) -> m (f (Meta s f a))
zonkWith fs0 tweak = go fs0 where
  go fs = fmap join . for fs $ \m -> do
    tweak m
    readMeta m >>= \mv -> case mv of
      Nothing  -> return (return m)
      Just fmf -> do
        tell $ Any True
        r <- go fmf
        r <$ writeMeta m r
{-# INLINE zonkWith #-}

zonkScope :: (MonadMeta s m, MonadWriter Any m, Traversable f, Monad f)
          => Scope b f (Meta s f a) -> m (Scope b f (Meta s f a))
zonkScope (Scope e) = Scope <$> (traverse.traverse) zonk e

zonkScope_ :: (MonadMeta s m, Traversable f, Monad f)
           => Scope b f (Meta s f a) -> m (Scope b f (Meta s f a))
zonkScope_ (Scope e) = Scope <$> (traverse.traverse) zonk_ e

------------------------------------------------------------------------------
-- Result
------------------------------------------------------------------------------

-- | The internal result type used by 'M'.
data Result a
  = Error !Diagnostic
  | OK !Int a

instance Functor Result where
  fmap f (OK n a) = OK n (f a)
  fmap _ (Error d) = Error d
  {-# INLINE fmap #-}

------------------------------------------------------------------------------
-- MonadMeta
------------------------------------------------------------------------------

class (Applicative m, MonadST m, s ~ World m) => MonadMeta s m | m -> s where
  askMeta :: m (MetaEnv s)
#ifndef HLINT
  default askMeta :: (m ~ t n, MonadTrans t, MonadMeta s n) => m (MetaEnv s)
  askMeta = lift askMeta
#endif

  localMeta :: (MetaEnv s -> MetaEnv s) -> m a -> m a

viewMeta :: MonadMeta s m => Getting a (MetaEnv s) a -> m a
viewMeta l = view l <$> askMeta

instance MonadMeta s m => MonadMeta s (Lazy.StateT t m) where
  localMeta f (Lazy.StateT m) = Lazy.StateT (localMeta f . m)

instance MonadMeta s m => MonadMeta s (Strict.StateT t m) where
  localMeta f (Strict.StateT m) = Strict.StateT (localMeta f . m)

instance MonadMeta s m => MonadMeta s (SharingT m) where
  localMeta f (SharingT m) = SharingT (localMeta f m)
  {-# INLINE localMeta #-}

------------------------------------------------------------------------------
-- M
------------------------------------------------------------------------------

-- | The unification monad provides a 'fresh' variable supply and tracks a current
-- 'Rendering' to blame for any unification errors.
newtype M s a = M { unM :: MetaEnv s -> ST s a }

instance Functor (M s) where
  fmap f (M m) = M (fmap f . m)
  {-# INLINE fmap #-}

instance Applicative (M s) where
  pure = return
  {-# INLINE pure #-}
  (<*>) = ap
  {-# INLINE (<*>) #-}

instance Monad (M s) where
  return = M . const . return
  {-# INLINE return #-}
  M m >>= k = M $ \ e -> do
    a <- m e
    unM (k a) e
  {-# INLINE (>>=) #-}
  fail s = M $ \e -> unsafeIOToST $ throwIO $! die e (pack s)

instance MonadST (M s) where
  type World (M s) = s
  liftST m = M $ const m
  {-# INLINE liftST #-}

instance MonadMeta s (M s) where
  askMeta = M $ \e -> return e
  {-# INLINE askMeta #-}
  localMeta f (M m) = M (m . f)
  {-# INLINE localMeta #-}

instance MonadMeta s m => MonadMeta s (MaybeT m) where
  askMeta = MaybeT $ Just <$> askMeta
  localMeta f = MaybeT . localMeta f . runMaybeT

instance Monoid m => Monoid (M s m) where
  mempty  = pure mempty
  mappend = liftA2 mappend

catchingST :: Getting (First a) SomeException a -> ST s r -> (a -> ST s r) -> ST s r
catchingST l m h = unsafeIOToST $ catchJust (preview l) (unsafeSTToIO m) (unsafeSTToIO . h)
{-# INLINE catchingST #-}

-- | Throw a 'Diagnostic' error.
throwM :: MonadST m => Diagnostic -> m a
throwM d = liftST $ unsafeIOToST (throwIO d)
{-# INLINE throwM #-}

-- | Evaluate an expression in the 'M' 'Monad' with a fresh variable supply.
runM :: Rendering -> (forall s. M s a) -> Either Diagnostic a
runM r m = runST $ do
  i <- newSTRef 0
  z <- newSTRef mempty
  catchingST _Diagnostic (Right <$> unM m (MetaEnv r i z)) (return . Left)
{-# INLINE runM #-}

-- | Evaluate an expression in the 'M' 'Monad' with a fresh variable supply, throwing any errors returned.
runM_ :: Rendering -> (forall s. M s a) -> a
runM_ r m = runST $ do
  i <- newSTRef 0
  z <- newSTRef mempty
  unM m (MetaEnv r i z)

-- | Evaluate an expression in the 'M' 'Monad' with a fresh variable supply, throwing any errors returned.
ioM :: MonadIO m => Rendering -> (forall s. M s a) -> m a
ioM r m = liftIO $ stToIO $ do
  i <- newSTRef 0
  z <- newSTRef mempty
  unM m (MetaEnv r i z)
{-# INLINE ioM #-}

-- | Generate a 'fresh' variable
fresh :: MonadMeta s m => m Int
fresh = do
  s <- viewMeta metaFresh
  liftST $ do
    i <- readSTRef s
    writeSTRef s $! i + 1
    return i
{-# INLINE fresh #-}

remember :: MonadMeta s m => Int -> TypeM s -> m ()
remember i t = do
  s <- viewMeta metaMemory
  liftST $ do
    m <- readSTRef s
    writeSTRef s $! m & at i ?~ t
{-# INLINE remember #-}
