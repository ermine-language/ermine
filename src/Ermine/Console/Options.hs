{-# LANGUAGE TemplateHaskell, DeriveDataTypeable #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett and Dan Doel 2012-2013
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Ermine.Console.Options
  ( MonitorOptions(..)
  , HasMonitorOptions(..)
  , Options(..)
  , HasOptions(..)
  , parseOptions
  ) where

import Control.Lens
import Data.Data
import Options.Applicative
import Paths_ermine

-- enable/disable EKG
data MonitorOptions = MonitorOptions
  { _monitorHost    :: String
  , _monitorPort    :: Int
  , _monitorEnabled :: Bool
  } deriving (Eq,Ord,Show,Read,Data,Typeable)

parseMonitorOptions :: Parser MonitorOptions
parseMonitorOptions = MonitorOptions
  <$> strOption (long "ekg-host" <> short 'h' <> help "host for the EKG server" <> metavar "HOST" <> action "hostname" <> value "localhost")
  <*> option (long "ekg-port" <> short 'p' <> help "port for the EKG server" <> metavar "PORT" <> value 5616)
  <*> (not <$> switch (long "no-ekg" <> help "do not start the EKG server" <> value False))

makeClassy ''MonitorOptions

data Options = Options
  { _optionsMonitorOptions :: MonitorOptions
  , _libdir :: FilePath
  , _files :: [FilePath]
  } deriving (Eq,Ord,Show,Read,Data,Typeable)

makeClassy ''Options

instance HasMonitorOptions Options where
  monitorOptions = optionsMonitorOptions

parseOptions :: IO (Parser Options)
parseOptions = do
  dd <- getDataDir
  return $ Options
       <$> parseMonitorOptions
       <*> option (long "libdir" <> short 'l' <> help "location of the ermine library" <> metavar "DIR" <> action "directory" <> value dd)
       <*> arguments Just (help "files" <> metavar "FILE" <> action "file")
