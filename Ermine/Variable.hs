module Ermine.Variable
  ( Variable(..)
  ) where

import Control.Lens

class Variable t where
  var :: Prism' (t a) a
