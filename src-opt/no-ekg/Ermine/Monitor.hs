{-# LANGUAGE CPP #-}
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
#ifdef EKG
  -- * Compatibilty with EKG
  , withServer
#endif
  ) where

import Control.Exception
import Control.Lens hiding (Setting)
import Control.Monad.Trans
import Control.Monad.Reader
import Data.ByteString.Lens
import Data.Data
import Data.Text
#ifndef EKG
import Data.Void
#endif
import Options.Applicative
import System.Process
#ifdef EKG
import System.Remote.Monitoring
import qualified System.Remote.Gauge as G
import qualified System.Remote.Counter as C
import qualified System.Remote.Label as L
#endif

-- | Enable/disable EKG

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
#ifdef EKG
parseMonitorOptions = MonitorOptions
  <$> strOption (long "ekg-host" <> short 'H' <> help "host for the EKG server" <> metavar "HOST" <> action "hostname" <> value "localhost")
  <*> option (long "ekg-port" <> short 'P' <> help "port for the EKG server" <> metavar "PORT" <> value 5616)
  <*> (not <$> switch (long "no-ekg" <> short 'Q' <> help "do NOT start the EKG server" <> value False))
  <*> switch (long "ekg-open" <> short 'M' <> help "open EKG on launch")
#else
parseMonitorOptions = return $ MonitorOptions "localhost" 5616 False False
#endif

data ShutdownMonitor = ShutdownMonitor deriving (Typeable, Show)

instance Exception ShutdownMonitor

data Monitor = Monitor
  { __monitorOptions :: MonitorOptions
#ifdef EKG
  , _monitorServer   :: Maybe Server
#endif
  }

makeClassy ''Monitor

instance HasMonitorOptions Monitor where
  monitorOptions = _monitorOptions

#ifdef EKG
withServer :: HasMonitor t => t -> (Server -> IO ()) -> IO ()
withServer t k = case t^.monitorServer of
  Nothing -> return ()
  Just s  -> k s
#endif

class Setting t a | t -> a where
  assign :: MonadIO m => t -> a -> m ()        -- set
  assign _ _ = return ()
  update :: MonadIO m => t -> (a -> a) -> m () -- modify
  update _ _ = return ()

#ifdef EKG

newtype Gauge = Gauge { runGauge :: Maybe G.Gauge }
newtype Label = Label { runLabel :: Maybe L.Label }
newtype Counter = Counter { runCounter :: Maybe C.Counter }

instance Setting Label Text where
  assign (Label t) a = liftIO $ maybe (return ()) (L.set ?? a) t
  update (Label t) f = liftIO $ maybe (return ()) (L.modify f) t

instance Setting Gauge Int where
  assign (Gauge t) a = liftIO $ maybe (return ()) (G.set ?? a) t
  update (Gauge t) f = liftIO $ maybe (return ()) (G.modify f) t

#else

data Gauge = Gauge Text
data Label = Label Text
data Counter = Counter Text

instance Setting Label Text
instance Setting Gauge Int

#endif

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
#ifdef EKG
  inc (Gauge t)   = liftIO $ maybe (return ()) G.inc t
  add (Gauge t) i = liftIO $ maybe (return ()) (G.add ?? i) t
#endif

instance Incremental Counter where
#ifdef EKG
  inc (Counter t)   = liftIO $ maybe (return ()) C.inc t
  add (Counter t) i = liftIO $ maybe (return ()) (C.add ?? i) t
#endif

gauge :: (MonadIO m, HasMonitor t) => Text -> t -> m Gauge
gauge = runReaderT . gaugeM

counter :: (MonadIO m, HasMonitor t) => Text -> t -> m Counter
counter = runReaderT . counterM

label :: (MonadIO m, HasMonitor t) => Text -> t -> m Label
label = runReaderT . labelM

-- | create a gauge
gaugeM :: (MonadIO m, MonadReader t m, HasMonitor t) => Text -> m Gauge
#ifdef EKG
gaugeM l = view monitorServer >>= maybe (return $ Gauge Nothing) (liftIO . fmap (Gauge . Just) . getGauge l)
#else
gaugeM = return . Gauge
#endif

-- | create a counter
counterM :: (MonadIO m, MonadReader t m, HasMonitor t) => Text -> m Counter
#ifdef EKG
counterM l = view monitorServer >>= maybe (return $ Counter Nothing) (liftIO . fmap (Counter . Just) . getCounter l)
#else
counterM = return . Counter
#endif

-- | create a label
labelM :: (MonadIO m, MonadReader t m, HasMonitor t) => Text -> m Label
#ifdef EKG
labelM t = view monitorServer >>= maybe (return $ Label Nothing) (liftIO . fmap (Label . Just) . getLabel t)
#else
labelM = return . Label
#endif

withMonitor :: HasMonitorOptions t => t -> (Monitor -> IO a) -> IO a
withMonitor t k
#ifdef EKG
  | t^.monitorEnabled = do
    server <- forkServer (t^.monitorHost.packedChars) (t^.monitorPort)
    let uri = monitorUri t
    putStrLn $ "Monitoring enabled at " ++ uri
    when (t^.monitorOpen) $ do
      _ <- system $ "/usr/bin/open " ++ uri
      return ()
    k (Monitor (t^.monitorOptions) $ Just server) `finally` throwTo (serverThreadId server) ShutdownMonitor
#endif
  | otherwise = k $ Monitor (t^.monitorOptions) Nothing
