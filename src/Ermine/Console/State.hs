{-# LANGUAGE TemplateHaskell #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett and Dan Doel 2012-2013
-- License   :  BSD2
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
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

-- | The monad in which we perform our console interactions
type Console = StateT ConsoleState IO

-- | The extra state we need in order to perform auto-completion in the console
newtype ConsoleState = ConsoleState
  { _consoleIds :: Set String -- ^ The set of extra names that are in scope for auto-completion.
  }

instance Default ConsoleState where
  def = ConsoleState mempty

makeClassy ''ConsoleState
