module Ermine.Syntax.Name where

import Control.Lens
import Data.Text

class HasName t where
  name :: Lens' t Text
