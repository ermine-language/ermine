{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) McGraw Hill Financial 2014
-- License   :  BSD2
-- Maintainer:  Stephen Compall <scompall@nocandysw.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------

module Ermine.Loader.Core
  ( -- * Loaders with cache states
    Loader(Loader)
  , LoadResult()
  , uncachedLoader
  , permCachedLoader
  , testBasedCacheLoader
  , load
  , reload
    -- * Manipulating loaders
  , loaded
  , thenM
  , thenM'
  , fanoutIdLoaderM
  , xmapCacheKey
  , contramapName
    -- * Combining multiple loaders
  , alwaysFail
  , orElse
  , composeLoaders
  , productLoader
  ) where

import Control.Applicative
import Control.Arrow
import Control.Lens
import Control.Monad
import Data.Data (Typeable)
import Data.Profunctor (Strong(..))
import Data.Profunctor.Rep (Representable(..))
import GHC.Generics (Generic)
import Data.Profunctor.Sieve(Sieve(..))

-- | A pure loader or reloader.  Whether you are loading or reloading
-- depends on whether an 'e' is passed.
--
-- 'a' is the "module name"; 'e' is the "freshness test" answered on
-- the side by an initial load, for example a timestamp for filesystem
-- loads; 'm' is the effect context of loading, e.g. '(MaybeT IO)' for
-- filesystem loads (where a module might not be found); 'b' is the
-- result of the load.
--
-- The simplest way to make a loader is with 'uncachedLoader'; from
-- there, an 'arr'-equivalent is obvious, though there is no lawful
-- 'Arrow' for 'Loader'.
data Loader e m a b = Loader
  { _load :: a -> m (e, b)
  , _reload :: a -> e -> m (Maybe (e, b))}
  deriving (Functor, Generic, Typeable)

data LoadResult e m b = LoadResult (m (e, b)) (e -> m (Maybe (e, b)))
  deriving (Functor, Generic, Typeable)

makeLenses ''Loader

instance Functor m => Profunctor (Loader e m) where
  lmap = contramapName
  rmap = fmap

instance Functor m => Strong (Loader e m) where
  first' (Loader l r) =
    Loader (\ ~(a, c) -> l a & mapped.mapped %~ (,c))
           (\ ~(a, c) -> r a >>> mapped.mapped.mapped %~ (,c))
  second' (Loader l r) =
    Loader (\ ~(c, a) -> l a & mapped.mapped %~ (c,))
           (\ ~(c, a) -> r a >>> mapped.mapped.mapped %~ (c,))

instance (Functor m) => Representable (Loader e m) where
  type Rep (Loader e m) = LoadResult e m

  tabulate = mapped %~ (\(LoadResult l r) -> (l, r))
             >>> view (from loadOrReload)

instance Functor m => Sieve (Loader e m) (LoadResult e m) where
  sieve l x = LoadResult (_load l x) (_reload l x)

-- | A more convenient representation of 'Loader' for writing
-- combinators.
loadOrReload :: Iso (Loader e m a b) (Loader e' m' a' b')
                    (a -> (m (e, b), e -> m (Maybe (e, b))))
                    (a' -> (m' (e', b'), e' -> m' (Maybe (e', b'))))
loadOrReload =
  iso (\(Loader l r) -> l &&& r) (\g -> Loader (fst . g) (snd . g))

-- | Alter the covariant parts of a 'Loader'.
setCovariant :: (m (e, b) -> f (e, c))
                -> (m (Maybe (e, b)) -> f (Maybe (e, c)))
                -> Loader e m a b -> Loader e f a c
setCovariant tl tr (Loader l r) =
  Loader (tl . l) ((tr .) . r)

-- | Lift a transformation on the results of a loader.  This has
-- various uses; for example, 'loaded lift' transforms the 'm' of a
-- 'Loader' into 't m' for a given 'MonadTrans t'; other natural
-- transformations are commonly needed to get two 'Loader's speaking
-- different languages to speak the same language.
loaded :: (forall t. m t -> f t) -> Loader e m a b -> Loader e f a b
loaded nt = setCovariant nt nt

-- | Lift a Kleisli arrow on the results of a loader, including the
-- cache key.
--
-- NB: flip thenM' return = id, but composeLoaders (uncachedLoader
-- return) /= id
thenM' :: Monad m => Loader e m a b -> ((e, b) -> m (e, c)) -> Loader e m a c
thenM' l f = setCovariant (>>= f) (>>= maybe (return Nothing) (liftM Just . f)) l

-- | Lift a Kleisli arrow on the results of a loader.
--
-- NB: flip thenM return = id, but composeLoaders (uncachedLoader
-- return) /= id
thenM :: Monad m => Loader e m a b -> (b -> m c) -> Loader e m a c
thenM l = thenM' l . runKleisli . second . Kleisli

-- | Pass through the argument.
fanoutIdLoaderM :: Monad m => Loader e m a b -> Loader e m a (a, b)
fanoutIdLoaderM (Loader l r) =
  Loader (\a -> l a & lifted.mapped %~ (a,))
         (\a e -> r a e & lifted.mapped.mapped %~ (a,))

-- | Lift a cache key isomorphism.
xmapCacheKey :: Functor m =>
                AnIso' e e2 -> Iso' (Loader e m a b) (Loader e2 m a b)
xmapCacheKey i = withIso i $ \t f -> iso (xf t f) (xf f t)
  where xf t f (Loader l r) =
          Loader (l & mapped.mapped._1 %~ t)
                 (r & fmap (over argument f
                            . over (mapped.mapped.mapped._1) t))

-- | A loader without reloadability.  This is the simplest way to make
-- a loader.
uncachedLoader :: Functor m => (a -> m b) -> Loader () m a b
uncachedLoader f = Loader (fmap ((),) . f)
                          (const . fmap (pure.pure) . f)

-- | A loader that only loads each value once, always telling the
-- caller that the old value is fresh enough on each subsequent
-- reload.
permCachedLoader :: Applicative m => (a -> m b) -> Loader () m a b
permCachedLoader f = Loader (fmap ((),) . f)
                            (const . const $ pure Nothing)

-- | A loader whose freshness is based on a monadic predicate.
testBasedCacheLoader ::
  Monad m
  => (a -> m x)        -- ^ Initial state for either load or reload.
  -> (x -> m b)        -- ^ Fresh loader.
  -> (x -> m e)        -- ^ Find a freshness value.
  -> (e -> e -> Bool)  -- ^ Given prior & new e, reload? (e.g. '(/=)')
  -> Loader e m a b
testBasedCacheLoader inits loadm getE cmpE =
  Loader (inits >=> runKleisli (Kleisli getE &&& Kleisli loadm))
         (\n cv -> do
             x <- inits n
             e <- getE x
             if cv `cmpE` e
               then (\b -> Just (e, b)) `liftM` loadm x
               else return Nothing)

-- | Contramap the name parameter.  A specialization of 'lmap' that
-- doesn't require the 'Functor' constraint.
contramapName :: (c -> a) -> Loader e m a b -> Loader e m c b
contramapName f = loadOrReload %~ (. f)

-- | Compose two loaders.  The loaders are assumed to have independent
-- ideas of the "freshness test".
--
-- NB: The right reloader is never used.  Therefore, this 'compose'
-- has no left identity.
composeLoaders :: Monad m => Loader e2 m b c -> Loader e m a b
                          -> Loader (e, e2) m a c
composeLoaders l2 =
  loadOrReload %~ \l1' n ->
  let l1 = fst (l1' n)
  in (do (e', a) <- l1
         view load l2 a & lifted._1 %~ (e',),
      \(_, e2) ->
      do (e', a) <- l1
         view reload l2 a e2 & lifted.mapped._1 %~ (e',))

-- | The product of two loaders.
--
-- NB: if one reloader returns Nothing, but not the other, the other's
-- loader is always run instead.
productLoader :: Monad m => Loader e m a c -> Loader e2 m b d
                 -> Loader (e, e2) m (a, b) (c, d)
productLoader (Loader l1 r1) (Loader l2 r2) =
  Loader ((l1 *** l2) >>> uncurry (liftM2 reorder)) reload'
  where reload' (a, b) (e, e2) = do
          r1r <- r1 a e
          r2r <- r2 b e2
          defaulting2 (l1 a) (l2 b) r1r r2r &
            lifted.mapped %~ uncurry reorder
        reorder (e, c) (e2, d) = ((e, e2), (c, d))

-- | Fall back to 'ma' or 'mb' iff 'Maybe a' xor 'Maybe b' is
-- undefined.  (Really needs just 'Applicative'.)
defaulting2 :: Monad m => m a -> m b -> Maybe a -> Maybe b -> m (Maybe (a, b))
defaulting2 ma mb = go
  where go Nothing Nothing = return Nothing
        go Nothing (Just b) = liftM (Just . (,b)) ma
        go (Just a) Nothing = liftM (Just . (a,)) mb
        go (Just a) (Just b) = return (Just (a, b))

-- | The 'orElse' identity; never succeeds.
alwaysFail :: Alternative m => Loader e m a b
alwaysFail = Loader (const empty) (const . const $ empty)

-- | Try the left loader, then the right loader.
--
-- NB: the reload will only try the loader that succeeded before.  We
-- *could* fall over to the other (fresh) loader if a given reloader
-- fails in 'm'; a successful 'm' of 'Nothing' indicates that the
-- "load" was successful, but the resource was fresh, so there is no
-- need to reload, and it didn't bother giving back an 'a'.  That's
-- not what happens now, though.
orElse :: Alternative m => Loader e m a b -> Loader e2 m a b ->
          Loader (Either e e2) m a b
orElse l1 =
  let mix ((fresh1, rel1), (fresh2, rel2)) =
        (over (mapped._1) Left fresh1
         <|> over (mapped._1) Right fresh2
        ,(over (mapped.mapped._1) Left . rel1)
         ||| (over (mapped.mapped._1) Right . rel2))
  in loadOrReload %~ \lr2 -> mix . (l1 ^. loadOrReload &&& lr2)
