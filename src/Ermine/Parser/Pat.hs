{-# LANGUAGE TupleSections #-}
--------------------------------------------------------------------
-- |
-- Module    :  Ermine.Parser.Pat
-- Copyright :  (c) Edward Kmett and Dan Doel 2013
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
-- This module provides the parser for terms
--------------------------------------------------------------------

module Ermine.Parser.Pat ( validate
                         , pat
                         , PP
                         ) where

import Control.Applicative
import Control.Lens hiding (op)
import Data.Foldable as Foldable
import Data.Void
import qualified Data.Set as Set
import Ermine.Builtin.Pat
import Ermine.Parser.Style
import Ermine.Parser.Type
import Text.Parser.Combinators
import Text.Parser.Token

validate :: (Functor m, Monad m, Ord v) => Binder v a -> (v -> m Void) -> m ()
validate b e =
  () <$ foldlM (\s n -> if n `Set.member` s then vacuous $ e n else return $ Set.insert n s)
               Set.empty
               (vars b)

type PP = P Ann String

varP :: (Monad m, TokenParsing m) => m PP
varP = ident termIdent <&> sigp ?? anyType

pat0 :: (Monad m, TokenParsing m) => m PP
pat0 = varP
   <|> _p <$ symbol "_"
   <|> parens (tupp <$> pats)

sigP :: (Monad m, TokenParsing m) => m PP
sigP = sigp <$> try (ident termIdent <* colon) <*> annotation

pat1 :: (Monad m, TokenParsing m) => m PP
pat1 = sigP <|> pat0

pats :: (Monad m, TokenParsing m) => m [PP]
pats = commaSep pat

pat :: (Monad m, TokenParsing m) => m PP
pat = pat1
