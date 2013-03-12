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
                         , pat0
                         ) where

import Control.Applicative
import Control.Monad
import Control.Lens hiding (op)
import Data.Foldable as Foldable
import qualified Data.Set as Set
import Ermine.Parser.Style
import Ermine.Parser.Type
import Ermine.Syntax.Pat
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
varP = (go ?? anyType) <$> ident termIdent
   <|> parens (go <$> ident termIdent <* colon <*> annotation)
 where
 go v ann = ([v], SigP ann)

pat0 :: (Monad m, TokenParsing m) => m BP
pat0 = varP
   <|> ([], WildcardP) <$ symbol "_"
