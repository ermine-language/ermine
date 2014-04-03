{-# LANGUAGE DeriveDataTypeable #-}
module Ermine.Monitor.Exception
  ( ShutdownMonitor(..)
  ) where

import Control.Exception
import Data.Data

data ShutdownMonitor = ShutdownMonitor deriving (Show,Typeable,Data)

instance Exception ShutdownMonitor
