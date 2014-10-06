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
  , loadOrReload
  , loaded
  , alwaysFresh
  ) where

import Control.Applicative
import Control.Lens

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
  { _loadOrReload :: n -> Maybe e -> m (e, a) }

makeLenses ''Loader

instance Functor m => Functor (Loader e n m) where
  fmap = over loaded . fmap . fmap

-- | Lift a transformation on the results of a loader.  This has
-- various uses; for example, 'loaded %~ lift' transforms the 'm' of a
-- 'Loader' into 't m' for a given 'MonadTrans t'; other natural
-- transformations are commonly needed to get two 'Loader's speaking
-- different languages to speak the same language.
loaded :: IndexPreservingSetter (Loader e n m a) (Loader e n f b)
                                (m (e, a)) (f (e, b))
loaded = cloneIndexPreservingSetter (loadOrReload.mapped.mapped)

-- | Lift a cache key isomorphism.
xmapCacheKey :: Functor m =>
                AnIso' e e2 -> Iso' (Loader e n m a) (Loader e2 n m a)
xmapCacheKey i = withIso i $ \t f -> iso (xf t f) (xf f t)
  where xf t f = loadOrReload.mapped %~ dimap (fmap f) (mapped._1 %~ t)

-- | A loader without reloadability.
alwaysFresh :: Functor m => (n -> m a) -> Loader () n m a
alwaysFresh f = Loader (const . fmap ((),) . f)

-- | Contramap the name parameter.
contramapName :: (n' -> n) -> Loader e n m a -> Loader e n' m a
contramapName f = loadOrReload %~ (. f)

-- | Compose two loaders.  The loaders are assumed to have independent
-- ideas of the "freshness test"; that is why 'Loader' doesn't form a
-- 'Category', while the version that existentializes the freshness
-- test does.
--
-- (Only really needs Bind.)
compose :: Monad m => Loader e2 a m b -> Loader e n m a
                   -> Loader (e, e2) n m b
compose (Loader r2) (Loader r1) =
  Loader (\n cv -> do
             (e', a) <- r1 n (fmap fst cv)
             r2 a (fmap snd cv) & lifted._1 %~ (e',))

-- | The product of two loaders.
--
-- (Only really needs Apply.)
product :: Applicative m => Loader e a m c -> Loader e2 b m d
                         -> Loader (e, e2) (a, b) m (c, d)
product (Loader l1) (Loader l2) =
  Loader (\(a, b) cv -> liftA2 (\(e, c) (e2, d) -> ((e, e2), (c, d)))
                               (l1 a (fmap fst cv)) (l2 b (fmap snd cv)))

-- | Try the left loader, then the right loader.
orElse :: Alternative m => Loader e a m b -> Loader e2 a m b ->
          Loader (Maybe e, Maybe e2) a m b
orElse = undefined
{-
orElse (Loader l1) (Loader l2) =
  Loader (\n cv -> (l1 n (fst =<< cv) & cvside (,snd =<< cv))
                    <|> (l2 n (snd =<< cv) & cvside (fst =<< cv,)))
  where cvside = over (lifted._1) . (. Just)
-}
