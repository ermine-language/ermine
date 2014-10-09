{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

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
import Ermine.Syntax.Hint
import Ermine.Syntax.Kind as Kind
import Ermine.Syntax.Type as Type

data Class k t = Class
               { _kindArgs :: [Hint]
               , _typeArgs :: [(Hint, Scope Int Kind k)]
               , _context  :: [Scope Int (TK k) t]
               }

makeClassy ''Class
