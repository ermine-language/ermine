{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}
module Ermine.Console.Command.Kind
  ( kindCommand
  ) where

import Control.Lens
import Control.Monad.IO.Class
import Data.Monoid
import Ermine.Console.Command.Common
import Ermine.Parser.Type
import Text.Trifecta.Parser
import System.Console.Terminfo.PrettyPrint

kindCommand :: Command
kindCommand = cmd "kind"
  & desc .~ "compute the kind of a type"
  & arg  ?~ "type"
  & body .~ \s -> case parseString typ mempty s of
      Success a   -> liftIO $ print a
      Failure doc -> liftIO $ displayLn doc
