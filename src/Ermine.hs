--------------------------------------------------------------------
-- |
-- Module    :  Ermine
-- Copyright :  (c) Edward Kmett 2011-12
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
-- This module provides a single export with most of the types
-- and terms involved (modulo conflicts) for easy testing
--------------------------------------------------------------------
module Ermine
  ( module Ermine.Core
  , module Ermine.Diagnostic
  , module Ermine.Digest
  , module Ermine.Global
  , module Ermine.Kind
  , module Ermine.Kind.Inference
  , module Ermine.Kind.Parser
  , module Ermine.Kind.Unification
  , module Ermine.Meta
  , module Ermine.Pat
  , module Ermine.Prim
  , module Ermine.Scope
  , module Ermine.Syntax
  , module Ermine.Term
  , module Ermine.Type
  , module Ermine.Type.Builtin
  , module Ermine.Type.Unification
  , module Ermine.Version
  ) where

import Ermine.Core hiding (Var, App, Let, Case, Lam, Prim)
import Ermine.Diagnostic
import Ermine.Digest
import Ermine.Global
import Ermine.Kind hiding (Var)
import Ermine.Kind.Inference
import Ermine.Kind.Unification
import Ermine.Kind.Parser
import Ermine.Meta
import Ermine.Pat
import Ermine.Prim
import Ermine.Scope
import Ermine.Syntax
import Ermine.Term hiding (Var, App, Loc)
import Ermine.Type hiding (Var, App, Loc, Tuple)
import Ermine.Type.Builtin
import Ermine.Type.Unification
import Ermine.Version
