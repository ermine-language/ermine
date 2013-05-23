{-# LANGUAGE TemplateHaskell #-}

module Ermine.Syntax.Class
  ( Class(Class)
  , kindArgs
  , typeArgs
  , context
  ) where

import Bound
import Control.Lens
import Data.Void
import Ermine.Syntax.Hint
import Ermine.Syntax.Kind as Kind
import Ermine.Syntax.Type as Type

data Class = Class { _kindArgs :: [Hint]
                   , _typeArgs :: [Hinted (Scope Int Kind Void)]
                   , _context  :: [Type Void Int]
                   }

makeLenses ''Class
