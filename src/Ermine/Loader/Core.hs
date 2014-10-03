{-# OPTIONS_GHC -Wall #-}
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
  ( Loader(Loader)
  , loadOrReload
  , alwaysFresh
  ) where

import Control.Lens

-- | A pure loader or reloader.
data Loader e n m a = Loader
  { _loadOrReload :: n -> Maybe e -> m (e, a) }

makeLenses ''Loader

instance Functor m => Functor (Loader e n m) where
  fmap = over loaded . fmap . fmap

-- | Lift a transformation on the results of a loader.
loaded :: IndexPreservingSetter (Loader e n m a) (Loader e n f b)
                                (m (e, a)) (f (e, b))
loaded = setting $ over (loadOrReload.mapped.mapped)

xmapCacheKey :: Functor m => AnIso' e e2 -> Loader e n m a -> Loader e2 n m a
xmapCacheKey i l = withIso i $ \t f ->
  l {_loadOrReload = (\q -> (mapped._1 %~ t) . q . fmap f) . _loadOrReload l}

-- | A loader without reloadability.
alwaysFresh :: Functor m => (n -> m a) -> Loader () n m a
alwaysFresh f = Loader (const . fmap ((),) . f)

-- | Contramap the name parameter.
contramapName :: (n' -> n) -> Loader e n m a -> Loader e n' m a
contramapName f = loadOrReload %~ (. f)

-- | Compose two loaders.
compose :: Monad m => Loader e2 a m b -> Loader e n m a
                   -> Loader (e, e2) n m b
compose (Loader r2) (Loader r1) =
  Loader (\n cv -> do
             (e', a) <- r1 n (cv ^? _Just._1)
             r2 a (cv ^? _Just._2) & lifted._1 %~ (e',))
