{-# LANGUAGE TemplateHaskell, DeriveDataTypeable #-}
--------------------------------------------------------------------
-- |
-- Module    :  Ermine.Diagnostic
-- Copyright :  (c) Edward Kmett and Dan Doel 2012
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
-- This module re-exports 'Rendering' from @trifecta@ along with
-- our in-house diagnostic type.
--
-- This allows the non-parsing modules to not have to concern
-- themselves with how we choose to represent source locations.
--------------------------------------------------------------------
module Ermine.Diagnostic
  (
  -- * Rendering
    Rendering
  , HasRendering(..)
  -- * Diagnostics
  , Diagnostic(Diagnostic)
  , AsDiagnostic(..)
  , Err(Err)
  , HasErr(..)
  , die
  ) where

import Control.Exception
import Control.Exception.Lens
import Control.Lens
import Data.Monoid
import Data.Set
import Data.Typeable
import Text.PrettyPrint.ANSI.Leijen
import Text.Trifecta.Rendering
import Text.Trifecta.Parser

-- | Ermine diagnostic type
data Diagnostic = Diagnostic !Rendering (Maybe Doc) [Doc] (Set String)
  deriving Typeable

-- | Construct a diagnostic
die :: HasRendering r => r -> String -> Diagnostic
die r s = Diagnostic (r^.rendering) (Just (pretty s)) [] mempty

-- | This provides the '_Diagnostic' prism that can be used when matching against 'SomeException'
-- or in a custom 'Error' type.
class AsDiagnostic t where
  _Diagnostic :: Prism' t Diagnostic

instance AsDiagnostic Diagnostic where
  _Diagnostic = id

instance AsDiagnostic SomeException where
  _Diagnostic = exception

instance Exception Diagnostic

instance Show Diagnostic where
  show d = show $ explain (d^.rendering) (d^.err)

instance HasErr Diagnostic where
  err f (Diagnostic r p a x) = f (Err p a x) <&> \(Err q b y) -> Diagnostic r q b y

instance HasRendering Diagnostic where
  rendering f (Diagnostic r p a x) = f r <&> \r' -> Diagnostic r' p a x
