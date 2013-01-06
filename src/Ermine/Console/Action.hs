{-# LANGUAGE TemplateHaskell #-}
module Ermine.Console.Action where

import Control.Lens

data Act = Act { _name :: String, _alts :: [String], _arg :: Maybe String, _desc :: String }

makeLenses ''Act
