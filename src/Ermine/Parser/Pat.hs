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
                         ) where

import Control.Applicative
import Control.Lens hiding (op)
import Data.Foldable as Foldable
import Data.Traversable as Traversable
import qualified Data.Set as Set
import Ermine.Parser.Style
import Ermine.Parser.Type
import Ermine.Syntax.Pat
import Ermine.Syntax.Literal
import Text.Parser.Combinators
import Text.Parser.Token

type BP = ([String], Pat Ann)

validate :: (Functor m, Monad m, Parsing m) => [String] -> m ()
validate ns =
  () <$ foldlM (\s n ->
                 if n `Set.member` s
                   then unexpected $ "duplicate bindings in pattern for: " ++ n
                   else return $ Set.insert n s)
               Set.empty
               ns

varP :: (Monad m, TokenParsing m) => m BP
varP = ident termIdent <&> \v -> ([v], SigP anyType)

pat0 :: (Monad m, TokenParsing m) => m BP
pat0 = varP
   <|> ([], WildcardP) <$ symbol "_"
   <|> parens (fmap tup . sequenceA <$> pats)
 where
 tup [p] = p
 tup ps  = TupP ps

sigp :: (Monad m, TokenParsing m) => m BP
sigp = f <$> try (ident termIdent <* colon) <*> annotation
 where f v ann = ([v], SigP ann)

pat1 :: (Monad m, TokenParsing m) => m BP
pat1 = sigp <|> pat0

pats :: (Monad m, TokenParsing m) => m [BP]
pats = commaSep pat

pat :: (Monad m, TokenParsing m) => m BP
pat = pat1
