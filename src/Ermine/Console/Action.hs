module Ermine.Console.Action where

import Control.Lens

data Action = Action { _name :: String, _alts :: [String], _arg :: Maybe String, _desc: String }

makeLenses ''Action
