--------------------------------------------------------------------
-- |
-- Module    :  Ermine.Type
-- Copyright :  (c) Edward Kmett and Dan Doel 2012
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
-- This module is a placeholder for Rendering from @trifecta@.
--
-- Eventually it'll be replaced with a more general location type.
--------------------------------------------------------------------
module Ermine.Rendering where

-- | This is a location in a source file, including whatever we want to be able to show.
data Rendering = Rendering -- !String !Int !Int
  deriving (Eq,Ord,Show,Read)
