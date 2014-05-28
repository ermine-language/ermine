
--------------------------------------------------------------------
-- |
-- Copyright :  (c) Dan Doel 2014
-- License   :  BSD3
-- Maintainer:  Dan Doel <dan.doel@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--------------------------------------------------------------------

module Ermine.Builtin.Head
  ( hlameI
  , hlameL
  ) where

import Ermine.Builtin.Type
import Ermine.Syntax.Head

hlameI, hlameL :: Head
hlameI = mkHead lame 0 [] [] [int]
hlameL = mkHead lame 0 [] [] [long]
