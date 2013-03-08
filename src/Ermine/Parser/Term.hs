{-# LANGUAGE TupleSections #-}
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

import Bound
import Control.Applicative
import Control.Lens hiding (op)
import Data.List (elemIndex)
import Ermine.Parser.Keywords
import Ermine.Parser.Type as Type
import Ermine.Syntax
import Ermine.Syntax.Prim
import Ermine.Syntax.Pat
import Ermine.Syntax.Term
import Ermine.Syntax.Type hiding (Var, App, Tuple)
import qualified Ermine.Syntax.Type as Type
import Text.Parser.Combinators
import Text.Parser.Token
import Text.Parser.Token.Style

type Tm = Term Ann String

anyType :: Ann
anyType = Annot 1 . Scope . Type.Var $ B 0

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
   <|> parens (tup <$> terms)
 where
 tup [x] = x
 tup xs  = apps (HardTerm . Prim . Tuple $ length xs) xs

term1 :: (Monad m, TokenParsing m) => m Tm
term1 = foldl1 App <$> some term0

term2 :: (Monad m, TokenParsing m) => m Tm
term2 = lam <|> term1

binding :: (Monad m, TokenParsing m) => m ([String], Ann)
binding = (, anyType) <$> some (ident termid)
      <|> parens ((,) <$> some (ident termid) <* colon <*> annotation)

bindings :: (Monad m, TokenParsing m) => m [(String, Ann)]
bindings = concatMap (\(l,a) -> (,a) <$> l) <$> some binding

lam :: (Monad m, TokenParsing m) => m Tm
lam = build <$> try (bindings <* reserve op "->") <*> term
 where
 build [          ] body = body
 build ((v,ann):vs) body = Lam (SigP ann) . abstract (`elemIndex` [v]) $ build vs body

literal :: (Monad m, TokenParsing m) => m Tm
literal = HardTerm . Prim . either (Int . fromIntegral) Double <$> naturalOrDouble

term :: (Monad m, TokenParsing m) => m Tm
term = term2

terms :: (Monad m, TokenParsing m) => m [Tm]
terms = sepBy term comma
