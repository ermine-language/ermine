{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Ermine.Monitor.Combinators
  ( Setting(..)
  , Updating(..)
  , Gauged(..)
  , Incremental(..)
  ) where

import Control.Monad.Trans
import Data.Int
import System.Metrics.Label

class Setting t a | t -> a where
  assign :: MonadIO m => t -> a -> m ()        -- set
  assign _ _ = return ()

class Updating t a | t -> a where
  update :: MonadIO m => t -> (a -> a) -> m () -- modify
  update _ _ = return ()

class Num a => Gauged t a | t -> a where
  dec :: MonadIO m => t -> m ()
  sub :: MonadIO m => t -> a -> m ()

class Incremental t where
  inc :: MonadIO m => t -> m ()
  inc _ = return ()

  add :: MonadIO m => t -> Int64 -> m ()
  add _ _ = return ()
