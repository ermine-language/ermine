--------------------------------------------------------------------
-- |
-- Copyright :  (c) Dan Doel 2013
-- License   :  BSD3
-- Maintainer:  Dan Doel <dan.doel@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Ermine.Pretty.Global
  ( prettyGlobal
  ) where

import Control.Lens
import Data.Text (unpack)
import Ermine.Pretty
import Ermine.Syntax.Global
import Ermine.Syntax.Name

prettyGlobal :: Global -> Doc
prettyGlobal (Global _ fix md nm) = case fix of
  Infix _ _ -> parens qualName
  Prefix  _ -> parens $ text "prefix" <+> qualName
  Postfix _ -> parens $ text "postfix" <+> qualName
  Idfix     -> qualName
 where
 qualName = text $ unpack (md^.name) ++ "." ++ unpack nm
