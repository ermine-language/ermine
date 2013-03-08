--------------------------------------------------------------------
-- |
-- Module    :  Ermine.Parser.Term
-- Copyright :  (c) Edward Kmett and Dan Doel 2013
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
-- This module provides the parser for terms
--------------------------------------------------------------------

module Ermine.Parser.Term where

import Control.Applicative
import Control.Lens hiding (op)
import Ermine.Parser.Keywords
import Ermine.Parser.Type as Type
import Ermine.Syntax.Prim
import Ermine.Syntax.Term
import Ermine.Syntax.Type as Type hiding (Var, App)
import Text.Parser.Combinators
import Text.Parser.Token
import Text.Parser.Token.Style

type Tm = Term (Annot (Maybe String) String) String

-- | The internal token style used for type variables
termid :: TokenParsing m => IdentifierStyle m
termid = haskellIdents
  & styleName  .~ "term variable"
  & styleReserved .~ keywords

op :: TokenParsing m => IdentifierStyle m
op = haskellOps

-- | Parse an atomic term
term0 :: (Monad m, TokenParsing m) => m Tm
term0 = Var <$> ident termid
   <|> literal
   <|> parens term

term1 :: (Monad m, TokenParsing m) => m Tm
term1 = foldl1 App <$> some term0

literal :: (Monad m, TokenParsing m) => m Tm
literal = HardTerm . Prim . either (Int . fromIntegral) Double <$> naturalOrDouble

term :: (Monad m, TokenParsing m) => m Tm
term = term1
