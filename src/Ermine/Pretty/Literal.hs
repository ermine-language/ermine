--------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett and Dan Doel 2013-2014
-- License   :  BSD2
-- Maintainer:  Dan Doel <dan.doel@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Ermine.Pretty.Literal
  ( prettyLiteral
  ) where

import Ermine.Pretty
import Ermine.Syntax.Literal

-- | Pretty print any 'Literal'.
prettyLiteral :: Literal -> Doc
prettyLiteral (Int i)    = text $ show i
prettyLiteral (Long l)   = text $ show l ++ "L"
prettyLiteral (Byte b)   = text $ show b ++ "B"
prettyLiteral (Short s)  = text $ show s ++ "S"
prettyLiteral (String s) = text $ show s
prettyLiteral (Integer i)= text $ show i ++ "I"
prettyLiteral (Char c)   = text $ show c
prettyLiteral (Float f)  = text $ show f ++ "F"
prettyLiteral (Double d) = text $ show d
