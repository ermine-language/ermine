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
-- License   :  BSD2
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
-- Meta variables
--------------------------------------------------------------------
module Ermine.Unification.Meta
  (
  -- * Meta variables
    Meta(Meta)
  , metaId, metaValue, metaRef, metaHint
  -- * Meta Types
  , MetaT, TypeM
  -- * Meta Kinds
  , MetaK, KindM
  -- ** λ-Depth
  , Depth, metaDepth, depthInf
  -- ** Union-By-Rank
  , Rank, metaRank, bumpRank
  -- ** Working with Meta
  , newMeta, newShallowMeta
  , readMeta
  , writeMeta
  -- ** Pruning
  , semiprune
  , zonk
  , zonk_
  , zonkWith
  , ultraZonk
  , ultraZonkWith
  , zonkScope
  , zonkScope_
  , hasSkolems
  , checkSkolems
  , checkEscapes
  , checkDistinct
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
import Data.IntSet.Lens (setOf)
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
import Data.Foldable (for_, traverse_)
import Data.Function (on)
import Data.IntMap
import Data.Monoid
import Data.STRef
import qualified Data.Set as Set
import Data.Text (pack)
import Data.Traversable
import Data.Word
import Ermine.Diagnostic
import Ermine.Syntax
import Ermine.Syntax.Hint
import Ermine.Syntax.Type
import Ermine.Syntax.Kind
import Ermine.Unification.Sharing

#ifdef HLINT
{-# ANN module "hlint: ignore Reduce duplication" #-}
#endif

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
           , _metaHint :: Hint
           , _metaId :: !Int
           , _metaRef :: !(STRef s (Maybe (f (Meta s f a))))
           , _metaDepth :: !(STRef s Depth)
           , _metaRank :: !(STRef s Rank)
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
type MetaK s = Meta s Kind Bool

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
  showsPrec d (Meta a _ i _ _ _) = showParen (d > 10) $
    showString "Meta " . showsPrec 11 a . showChar ' ' . showsPrec 11 i . showString " ..."

instance Eq (Meta s f a) where
  (==) = (==) `on` view metaId
  {-# INLINE (==) #-}

instance Ord (Meta s f a) where
  compare = compare `on` view metaId
  {-# INLINE compare #-}

-- | Construct a new meta variable
newMeta :: MonadMeta s m => a -> Hint -> m (Meta s f a)
newMeta a h = Meta a h <$> fresh
                       <*> liftST (newSTRef Nothing)
                       <*> liftST (newSTRef depthInf)
                       <*> liftST (newSTRef 0)
{-# INLINE newMeta #-}

-- | Construct a new meta variable at a given depth
newShallowMeta :: MonadMeta s m => Depth -> a -> Hint -> m (Meta s f a)
newShallowMeta d a h =
  Meta a h <$> fresh
           <*> liftST (newSTRef Nothing)
           <*> liftST (newSTRef d)
           <*> liftST (newSTRef 0)
{-# INLINE newShallowMeta #-}

-- | Read a meta variable
readMeta :: MonadMeta s m => Meta s f a -> m (Maybe (f (Meta s f a)))
readMeta (Meta _ _ _ r _ _) = liftST $ readSTRef r
{-# INLINE readMeta #-}

-- | Write to a meta variable
writeMeta :: MonadMeta s m => Meta s f a -> f (Meta s f a) -> m ()
writeMeta (Meta _ _ _ r _ _) a = liftST $ writeSTRef r (Just a)
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

ultraZonk :: forall s m f v a
           . (MonadMeta s m, MonadWriter Any m, Traversable f, Monad f)
          => ATraversal v (f v) (Meta s f a) (f (Meta s f a))
          -> f v -> m (f v)
ultraZonk trav0 fvs0 = ultraZonkWith trav0 fvs0 $ \_ -> return ()
{-# INLINE ultraZonk #-}

ultraZonkWith :: forall s m f v a
               . (MonadMeta s m, MonadWriter Any m, Traversable f, Monad f)
              => ATraversal v (f v) (Meta s f a) (f (Meta s f a))
              -> f v -> (Meta s f a -> m ()) -> m (f v)
ultraZonkWith trav0 fvs0 tweak = go trav0 fvs0 where
  go :: forall u
      . ATraversal u (f u) (Meta s f a) (f (Meta s f a))
     -> f u -> m (f u)
  go trav fs = fmap join . for fs . cloneTraversal trav $ \m -> do
    tweak m
    readMeta m >>= \mv -> case mv of
      Nothing -> return (return m)
      Just fmf -> do
        tell $ Any True
        r <- go id fmf
        r <$ writeMeta m r
{-# INILINE ultraZonkWith #-}

zonkScope :: (MonadMeta s m, MonadWriter Any m, Traversable f, Monad f)
          => Scope b f (Meta s f a) -> m (Scope b f (Meta s f a))
zonkScope (Scope e) = Scope <$> (traverse.traverse) zonk e

zonkScope_ :: (MonadMeta s m, Traversable f, Monad f)
           => Scope b f (Meta s f a) -> m (Scope b f (Meta s f a))
zonkScope_ (Scope e) = Scope <$> (traverse.traverse) zonk_ e

-- | Check for escaped Skolem variables in any container full of types.
--
-- Dies due to an escaped Skolem or returns the container with all of its types fully zonked.
checkSkolems
  :: MonadMeta s m
  => Maybe Depth -> LensLike' m ts (TypeM s) -> [MetaT s] -> ts -> m ts
checkSkolems md trav sks ts = do
  for_ md $ checkEscapes ?? sks
  hasSkolems trav sks ts

-- | Checks if any of the listed skolems exists in the types given in the
-- traversal.
hasSkolems :: (Traversable f, Monad f, MonadMeta s m) => LensLike' m ts (f (Meta s f a)) -> [Meta s f a] -> ts -> m ts
hasSkolems trav sks = trav $ \t -> runSharing t $ zonkWith t tweak where
  skids = setOf (traverse.metaId) sks
  tweak v = when (has (ix $ v^.metaId) skids) $ fail "returning skolem"

-- | Checks if any of the skolems in the list escapes into the context defined
-- by the given depth.
checkEscapes
  :: MonadMeta s m
  => Depth -> [Meta s f a] -> m ()
checkEscapes d =
  traverse_ $ \s -> liftST (readSTRef $ s^.metaDepth) >>= \d' ->
  when (d' < d) $ fail "skolem escapes to environment"

checkDistinct
  :: (Traversable f, Monad f, Variable f, MonadMeta s m)
  => [Meta s f a] -> m [Meta s f a]
checkDistinct xs = do
  ys <- for xs $ withSharing zonk . return
  zs <- for ys $ maybe (fail "bound skolem") pure . preview _Var
  zs <$ unless (Set.size (Set.fromList zs) == length zs) (fail "indistinct skolems")

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
  pure = M . const . return
  {-# INLINE pure #-}
  (<*>) = ap
  {-# INLINE (<*>) #-}

instance Monad (M s) where
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

instance Semigroup m => Semigroup (M s m) where
  (<>) = liftA2 (<>)

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
