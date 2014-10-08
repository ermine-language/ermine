{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) McGraw Hill Financial 2014
-- License   :  BSD3
-- Maintainer:  Stephen Compall <scompall@nocandysw.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------

module Ermine.Loader.Core
  ( -- * Loaders with cache states
    Loader(Loader)
  , load
  , reload
  , loaded
  , alwaysFresh
  , alwaysFail
  , orElse
  ) where

import Control.Applicative
import Control.Arrow
import Control.Lens
import Control.Monad

-- | A pure loader or reloader.  Whether you are loading or reloading
-- depends on whether an 'e' is passed.
--
-- 'n' is the "module name"; 'e' is the "freshness test" answered on
-- the side by an initial load, for example a timestamp for filesystem
-- loads; 'm' is the effect context of loading, e.g. '(MaybeT IO)' for
-- filesystem loads (where a module might not be found); 'a' is the
-- result of the load.
--
-- 'Loader' forms a 'Profunctor' over 'n' and 'a' where 'Functor m'.
data Loader e n m a = Loader
  { _load :: n -> m (e, a)
  , _reload :: n -> e -> m (Maybe (e, a))}

makeLenses ''Loader

instance Functor m => Functor (Loader e n m) where
  fmap f = setCovariant (fmap (fmap f)) (fmap (fmap (fmap f)))

-- | A more convenient representation of 'Loader' for writing
-- combinators.
loadOrReload :: Iso (Loader e n m a) (Loader e' n' m' a')
                    (n -> (m (e, a), e -> m (Maybe (e, a))))
                    (n' -> (m' (e', a'), e' -> m' (Maybe (e', a'))))
loadOrReload =
  iso (\(Loader l r) -> l &&& r) (\g -> Loader (fst . g) (snd . g))

-- | Alter the covariant parts of a 'Loader'.
setCovariant :: (m (e, a) -> f (e, b))
                -> (m (Maybe (e, a)) -> f (Maybe (e, b)))
                -> Loader e n m a -> Loader e n f b
setCovariant tl tr (Loader l r) =
  Loader (tl . l) ((tr .) . r)

-- | Lift a transformation on the results of a loader.  This has
-- various uses; for example, 'loaded lift' transforms the 'm' of a
-- 'Loader' into 't m' for a given 'MonadTrans t'; other natural
-- transformations are commonly needed to get two 'Loader's speaking
-- different languages to speak the same language.
loaded :: (forall t. m t -> f t) -> Loader e n m a -> Loader e n f a
loaded nt = setCovariant nt nt

-- | Lift a cache key isomorphism.
xmapCacheKey :: Functor m =>
                AnIso' e e2 -> Iso' (Loader e n m a) (Loader e2 n m a)
xmapCacheKey i = withIso i $ \t f -> iso (xf t f) (xf f t)
  where xf t f (Loader l r) =
          Loader (l & mapped.mapped._1 %~ t)
                 (r & fmap (over argument f
                            . over (mapped.mapped.mapped._1) t))

-- | A loader without reloadability.
alwaysFresh :: Functor m => (n -> m a) -> Loader () n m a
alwaysFresh f = Loader (fmap ((),) . f)
                       (const . fmap (pure.pure) . f)

-- | Contramap the name parameter.
contramapName :: (n' -> n) -> Loader e n m a -> Loader e n' m a
contramapName f = loadOrReload %~ (. f)

-- | Compose two loaders.  The loaders are assumed to have independent
-- ideas of the "freshness test"; that is why 'Loader' doesn't form a
-- 'Category', while the version that existentializes the freshness
-- test does.
--
-- NB: if the first reloader returns Nothing, the second loader will
-- not be tried; it assumes that if the first loader says the cache is
-- fresh, the second will as well
compose :: Monad m => Loader e2 a m b -> Loader e n m a
                   -> Loader (e, e2) n m b
compose l2 =
  loadOrReload %~ \l1' n ->
  let (l1, r1) = l1' n
  in (do (e', a) <- l1
         (l2 ^. load) a & lifted._1 %~ (e',),
      -- TODO if existing composes are cheap on the right, only use
      -- the load from the compose
      \(e, e2) ->
      r1 e >>= maybe (return Nothing)
                     (\(e', a) ->
                       (l2 ^. reload) a e2 & lifted.mapped._1 %~ (e',)))

-- | The product of two loaders.
--
-- NB: if one reloader returns Nothing, but not the other, the other's
-- loader is always run instead.
product :: Monad m => Loader e a m c -> Loader e2 b m d
                   -> Loader (e, e2) (a, b) m (c, d)
product (Loader l1 r1) (Loader l2 r2) =
  Loader ((l1 *** l2) >>> uncurry (liftM2 reorder)) reload'
  where reload' = (\(a, b) (e, e2) -> do
                      r1r <- r1 a e
                      r2r <- r2 b e2
                      defaulting2 (l1 a) (l2 b) r1r r2r) &
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
alwaysFail :: Alternative m => Loader e a m b
alwaysFail = Loader (const empty) (const . const $ empty)

-- | Try the left loader, then the right loader.
--
-- NB: the reload will only try the loader that succeeded before.  We
-- *could* fall over to the other (fresh) loader if a given reloader
-- fails in 'm'; a successful 'm' of 'Nothing' indicates that the
-- "load" was successful, but the resource was fresh, so there is no
-- need to reload, and it didn't bother giving back an 'a'.  That's
-- not what happens now, though.
orElse :: Alternative m => Loader e a m b -> Loader e2 a m b ->
          Loader (Either e e2) a m b
orElse l1 =
  let mix ((fresh1, rel1), (fresh2, rel2)) =
        (over (mapped._1) Left fresh1
         <|> over (mapped._1) Right fresh2
        ,(over (mapped.mapped._1) Left . rel1)
         ||| (over (mapped.mapped._1) Right . rel2))
  in loadOrReload %~ \lr2 -> mix . (l1 ^. loadOrReload &&& lr2)
