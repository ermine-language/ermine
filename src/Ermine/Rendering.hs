--------------------------------------------------------------------
-- |
-- Module    :  Ermine.Rendering
-- Copyright :  (c) Edward Kmett and Dan Doel 2012
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
-- This module re-exports 'Rendering' from @trifecta@.
--
-- This allows the non-parsing modules to not have to concern
-- themselves with how we choose to represent source locations.
--------------------------------------------------------------------
module Ermine.Rendering
  ( Rendering 
  ) where

import Text.Trifecta.Rendering
