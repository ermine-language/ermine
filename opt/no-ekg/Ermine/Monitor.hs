{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Ermine.Monitor
  ( withMonitor
  -- * The Monitor
  , Monitor(..)
  , HasMonitor(..)
  -- * Gauges
  , Gauge(..)
  , gauge, gaugeM
  -- * Counters
  , Counter(..)
  , counter, counterM
  -- * Labels
  , Label(..)
  , label, labelM
  -- * Compatibilty with EKG
  , Server
  , withServer
  , forkServer
  -- * Options
  , module Ermine.Monitor.Options
  -- * Modifiers
  , module Ermine.Monitor.Combinators
  ) where

import Control.Lens hiding (Setting)
import Control.Monad.Trans
import Control.Monad.Reader
import Data.Foldable as F
import Data.Text
import Data.ByteString (ByteString)
import Ermine.Monitor.Combinators
import Ermine.Monitor.Options

data Server = Server

data Monitor = Monitor
  { __monitorOptions :: MonitorOptions
  , _monitorServer :: Maybe Server
  }

makeClassy ''Monitor

instance HasMonitorOptions Monitor where
  monitorOptions = _monitorOptions

withServer :: HasMonitor t => t -> (Server -> IO ()) -> IO ()
withServer t = F.forM_ $ t^.monitorServer

forkServer :: ByteString -> Int -> IO Server
forkServer _ _ = return Server

data Gauge = Gauge Text
data Label = Label Text
data Counter = Counter Text

instance Updating Label Text
instance Setting Label Text
instance Setting Gauge Int where
  dec _ = return ()
  sub _ _ = return ()

instance Incremental Gauge
instance Incremental Counter

gauge :: (MonadIO m, HasMonitor t) => Text -> t -> m Gauge
gauge = runReaderT . gaugeM

counter :: (MonadIO m, HasMonitor t) => Text -> t -> m Counter
counter = runReaderT . counterM

label :: (MonadIO m, HasMonitor t) => Text -> t -> m Label
label = runReaderT . labelM

-- | create a gauge
gaugeM :: (MonadIO m, MonadReader t m, HasMonitor t) => Text -> m Gauge
gaugeM = return . Gauge

-- | create a counter
counterM :: (MonadIO m, MonadReader t m, HasMonitor t) => Text -> m Counter
counterM = return . Counter

-- | create a label
labelM :: (MonadIO m, MonadReader t m, HasMonitor t) => Text -> m Label
labelM = return . Label

withMonitor :: HasMonitorOptions t => t -> (Monitor -> IO a) -> IO a
withMonitor t k = k $ Monitor (t^.monitorOptions) Nothing
