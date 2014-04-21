module Ermine.Pretty.Id
  ( prettyId
  ) where

import Ermine.Pretty (Doc)
import Ermine.Pretty.Global
import Ermine.Pretty.Core
import Ermine.Syntax.Id

prettyId :: Id -> Doc
prettyId (GlobalId g) = prettyGlobal g
prettyId (InstanceId h) = prettyHead h
