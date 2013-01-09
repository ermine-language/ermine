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
-- This module re-exports 'Rendering' from @trifecta@.
--
-- This allows the non-parsing modules to not have to concern
-- themselves with how we choose to represent source locations.
--------------------------------------------------------------------
module Ermine.Diagnostic
  ( Rendering
  , Diagnostic(..)
  ) where

import Control.Exception
import Control.Exception.Lens
import Control.Lens
import Data.Typeable
import Data.Set
import Text.Trifecta.Rendering
import Text.Trifecta.Parser
import System.Console.Terminfo.PrettyPrint

data Diagnostic = Diagnostic !Rendering (Maybe TermDoc) [TermDoc] (Set String)
  deriving Typeable

class AsDiagnostic t where
  diagnostic :: Prism' t Diagnostic

instance AsDiagnostic Diagnostic where
  diagnostic = id

instance AsDiagnostic SomeException where
  diagnostic = exception

instance Exception Diagnostic

instance Show Diagnostic where
  show d = show $ explain (d^.rendering) (d^.err)

instance HasErr Diagnostic where
  err f (Diagnostic r p a x) = f (Err p a x) <&> \(Err q b y) -> Diagnostic r q b y

instance HasRendering Diagnostic where
  rendering f (Diagnostic r p a x) = f r <&> \r' -> Diagnostic r' p a x
