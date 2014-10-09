--------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett and Dan Doel 2012-2013
-- License   :  BSD2
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Ermine.Parser.Keywords where

import Data.HashSet
import Data.Monoid

-- | This is the set of keywords that can only occur at the beginning of the line for auto-completion purposes.
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

-- | This is the set of keywords that can occur anywhere on the line for auto-completion purposes.
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

-- | The set of all keywords.
--
-- @'keywords' = 'startingKeywords' '<>' 'otherKeywords'@
keywords :: HashSet String
keywords = startingKeywords <> otherKeywords
