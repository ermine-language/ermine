--------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett and Dan Doel 2014
-- License   :  BSD2
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
----------------------------------------------------------------------
module Ermine.Syntax.Name
  ( HasName(..)
  ) where

import Control.Lens
import Data.Text

class HasName t where
  name :: Lens' t Text
