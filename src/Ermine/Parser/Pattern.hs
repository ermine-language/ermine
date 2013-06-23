{-# LANGUAGE TupleSections #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett and Dan Doel 2013
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
-- This module provides the parser for terms
--------------------------------------------------------------------
module Ermine.Parser.Pattern
  ( validate
  , pattern
  , pattern0
  , PP
  ) where

import Control.Applicative
import Control.Lens hiding (op)
import Data.Foldable as Foldable
import Data.Text (Text)
import Data.Void
import qualified Data.Set as Set
import Ermine.Builtin.Pattern
import Ermine.Parser.Style
import Ermine.Parser.Type
import Ermine.Syntax
import Text.Parser.Combinators
import Text.Parser.Token

validate :: (Functor m, Monad m, Ord v) => Binder v a -> (v -> m Void) -> m ()
validate b e =
  () <$ foldlM (\s n -> if n `Set.member` s then vacuous $ e n else return $ Set.insert n s)
               Set.empty
               (vars b)

type PP = P Ann Text

varP :: (Monad m, TokenParsing m) => m PP
varP = termIdentifier <&> sigp ?? anyType

pattern0 :: (Monad m, TokenParsing m) => m PP
pattern0 = varP
   <|> _p <$ symbol "_"
   <|> parens (tup <$> patterns)

sigP :: (Monad m, TokenParsing m) => m PP
sigP = sigp <$> try (termIdentifier <* colon) <*> annotation

pattern1 :: (Monad m, TokenParsing m) => m PP
pattern1 = asp <$> try (termIdentifier <* symbol "@") <*> pattern1 <|> pattern0

pattern2 :: (Monad m, TokenParsing m) => m PP
pattern2 = sigP <|> pattern0

patterns :: (Monad m, TokenParsing m) => m [PP]
patterns = commaSep pattern2

pattern :: (Monad m, TokenParsing m) => m PP
pattern = pattern1
