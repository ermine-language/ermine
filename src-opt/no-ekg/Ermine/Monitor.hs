{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Ermine.Monitor
  ( MonitorOptions(..)
  , HasMonitorOptions(..)
  , parseMonitorOptions
  , withMonitor
  -- * The Monitor
  , Monitor(..)
  , HasMonitor(..)
  , monitorUri
  -- * Gauges
  , Gauge(..)
  , gauge, gaugeM
  -- * Counters
  , Counter(..)
  , counter, counterM
  -- * Labels
  , Label(..)
  , label, labelM
  -- * Modifiers
  , Setting(..)
  , Incremental(..)
  , dec
  , sub
-- #ifdef EKG
--   -- * Compatibilty with EKG
--   , withServer
-- #endif
  ) where

import Control.Exception
import Control.Lens hiding (Setting)
import Control.Monad.Trans
import Control.Monad.Reader
import Data.Data
import Data.Text
import Options.Applicative

data MonitorOptions = MonitorOptions
  { _monitorHost    :: String
  , _monitorPort    :: Int
  , _monitorEnabled :: Bool
  , _monitorOpen    :: Bool
  } deriving (Eq,Ord,Show,Read,Data,Typeable)

makeClassy ''MonitorOptions

monitorUri :: HasMonitorOptions t => t -> String
monitorUri t = "http://" ++ t^.monitorHost ++ ":" ++ show (t^.monitorPort) ++ "/"

-- | Parse EKG configuration
parseMonitorOptions :: Parser MonitorOptions
parseMonitorOptions = pure $ MonitorOptions "localhost" 5616 False False

data ShutdownMonitor = ShutdownMonitor deriving (Typeable, Show)

instance Exception ShutdownMonitor

data Monitor = Monitor
  { __monitorOptions :: MonitorOptions
  }

makeClassy ''Monitor

instance HasMonitorOptions Monitor where
  monitorOptions = _monitorOptions

-- #ifdef EKG
-- withServer :: HasMonitor t => t -> (Server -> IO ()) -> IO ()
-- withServer t k = case t^.monitorServer of
--   Nothing -> return ()
--   Just s  -> k s
-- #endif

class Setting t a | t -> a where
  assign :: MonadIO m => t -> a -> m ()        -- set
  assign _ _ = return ()
  update :: MonadIO m => t -> (a -> a) -> m () -- modify
  update _ _ = return ()

data Gauge = Gauge Text
data Label = Label Text
data Counter = Counter Text

instance Setting Label Text
instance Setting Gauge Int

dec :: (MonadIO m, Setting t a, Num a) => t -> m ()
dec t = update t (subtract 1)

sub :: (MonadIO m, Setting t a, Num a) => t -> a -> m ()
sub t n = update t (subtract n)

class Incremental t where
  inc :: MonadIO m => t -> m ()
  inc _ = return ()

  add :: MonadIO m => t -> Int -> m ()
  add _ _ = return ()

instance Incremental Gauge where
instance Incremental Counter where

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
withMonitor t k = k $ Monitor (t^.monitorOptions)
