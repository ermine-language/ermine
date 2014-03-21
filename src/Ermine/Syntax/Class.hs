{-# LANGUAGE TemplateHaskell #-}

--------------------------------------------------------------------
---- |
---- Copyright :  (c) Edward Kmett and Dan Doel 2013-2014
---- License   :  BSD3
---- Maintainer:  Edward Kmett <ekmett@gmail.com>
---- Stability :  experimental
---- Portability: non-portable (DeriveDataTypeable)
----
----------------------------------------------------------------------

module Ermine.Syntax.Class
  ( Class(Class)
  , HasClass(..)
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

makeClassy ''Class
