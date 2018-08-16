{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Ermine.Monitor.Options
  ( MonitorOptions(..)
  , HasMonitorOptions(..)
  , parseMonitorOptions
  , monitorUri
  ) where

import Control.Lens hiding (Setting)
import Data.Data
import Data.Semigroup ((<>))
import Options.Applicative

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
parseMonitorOptions = MonitorOptions
  <$> strOption (long "ekg-host" <> short 'H' <> help "host for the EKG server" <> metavar "HOST" <> action "hostname" <> value "localhost")
  <*> option auto (long "ekg-port" <> short 'P' <> help "port for the EKG server" <> metavar "PORT" <> value 5616)
  <*> (not <$> switch (long "no-ekg" <> short 'Q' <> help "do NOT start the EKG server"))
  <*> switch (long "ekg-open" <> short 'M' <> help "open EKG on launch")
