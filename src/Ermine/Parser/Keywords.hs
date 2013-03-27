--------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett and Dan Doel 2012-2013
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Ermine.Parser.Keywords where

import Data.HashSet
import Data.Monoid

startingKeywords :: HashSet String
startingKeywords = fromList
  [ "abstract"
  , "class"
  , "data"
  , "database"
  , "export"
  , "field"
  , "foreign"
  , "import"
  , "instance"
  , "private"
  , "type"
  ]

otherKeywords :: HashSet String
otherKeywords = fromList
  [ "case"
  , "constraint"
  , "constructor"
  , "do"
  , "exists"
  , "forall"
  , "hole"
  , "in"
  , "infix"
  , "infixl"
  , "infixr"
  , "let"
  , "of"
  , "phi"
  , "postfix"
  , "prefix"
  , "rho"
  , "subtype"
  , "table"
  , "where"
  , "_"
  , "Γ"
  , "ρ"
  , "φ"
  ]

keywords :: HashSet String
keywords = startingKeywords <> otherKeywords
