{-# LANGUAGE TemplateHaskell #-}
module Ermine.Console.State
  ( Console
  , ConsoleState(..)
  , HasConsoleState(..)
  ) where

import Control.Lens
import Control.Monad.State.Strict
import Data.Default
import Data.Set as Set
import Data.Monoid

type Console = StateT ConsoleState IO

newtype ConsoleState = ConsoleState
  { _consoleIds :: Set String
  }

instance Default ConsoleState where
  def = ConsoleState mempty

makeClassy ''ConsoleState
