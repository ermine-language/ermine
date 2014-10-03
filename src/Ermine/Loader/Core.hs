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
  (
  ) where

import Control.Lens
import Control.Monad

-- | A pure loader or reloader.
data Loader e n m a = Loader
  { _load :: n -> m (e, a)
  , _reload :: n -> e -> m (e, a)
  }

makeLenses ''Loader

instance Functor m => Functor (Loader e n m) where
  fmap = over loaded . fmap . fmap

-- | Lift a transformation on the results of a loader.
loaded :: IndexPreservingSetter (Loader e n m a) (Loader e n f b)
                                (m (e, a)) (f (e, b))
loaded = setting go
  where go f l = l {_load = f . _load l,
                    _reload = fmap f . _reload l}

xmapCacheKey :: Functor m => AnIso' e e2 -> Loader e n m a -> Loader e2 n m a
xmapCacheKey i l = withIso i $ \t f ->
  l {_load = _load l & mapped.mapped._1 %~ t,
     _reload = (\q -> (mapped._1 %~ t) . q . f) . _reload l}

-- | A loader without reloadability.
alwaysFresh :: Functor m => (n -> m a) -> Loader () n m a
alwaysFresh f = Loader (fmap ((),) . f) (const . fmap ((),) . f)

-- | Contramap the name parameter.
contramapName :: (n' -> n) -> Loader e n m a -> Loader e n' m a
contramapName f l = l {_load = _load l . f,
                       _reload = _reload l . f}

-- | Compose two loaders.
compose :: Monad m => Loader e2 a m b -> Loader e n m a
                   -> Loader (e, e2) n m b
compose (Loader {_load = l2, _reload = r2})
        (Loader {_load = l1, _reload = r1}) =
  Loader (l1 >=> \(e, a) -> l2 a & lifted._1 %~ (e,))
         (\n (e, e2) -> do
             (e', a) <- r1 n e
             r2 a e2 & lifted._1 %~ (e',))
