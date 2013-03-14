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

module Ermine.Parser.Term ( anyType
                          , term
                          , terms
                          ) where

import Bound
import Control.Applicative
import Data.List (elemIndex)
import Data.Traversable
import Ermine.Parser.Style
import Ermine.Parser.Type as Type
import Ermine.Parser.Pat
import Ermine.Syntax
import Ermine.Syntax.Literal
import Ermine.Syntax.Pat
import Ermine.Syntax.Term
import Text.Parser.Combinators
import Text.Parser.Token

type Tm = Term Ann String

-- | Parse an atomic term
term0 :: (Monad m, TokenParsing m) => m Tm
term0 = Var <$> ident termIdent
   <|> literal
   <|> parens (tup <$> terms)
 where
 tup [x] = x
 tup xs  = apps (HardTerm . Tuple $ length xs) xs

term1 :: (Monad m, TokenParsing m) => m Tm
term1 = match
    <|> foldl1 App <$> some term0

sig :: (Monad m, TokenParsing m) => m Tm
sig = build <$> term1 <*> optional (colon *> annotation)
 where
 build tm Nothing  = tm
 build tm (Just t) = Sig tm t

alt :: (Monad m, TokenParsing m) => m (Alt Ann (Term Ann) String)
alt = do (vs, p) <- pat
         reserve op "->"
         body <- term
         Alt p (abstract (`elemIndex` vs) body) <$ validate vs

match :: (Monad m, TokenParsing m) => m Tm
match = Case <$ symbol "case" <*> term <* symbol "of" <*> braces (semiSep alt)

term2 :: (Monad m, TokenParsing m) => m Tm
term2 = lam <|> sig

bindings :: (Monad m, TokenParsing m) => m ([String], [Pat Ann])
bindings = do p@(vs, _) <- sequenceA <$> some pat
              validate vs
              return p

lam :: (Monad m, TokenParsing m) => m Tm
lam = build <$> try (bindings <* reserve op "->") <*> term
 where
 build (vs, ps) body = Lam ps $ abstract (`elemIndex` vs) body

literal :: (Monad m, TokenParsing m) => m Tm
literal = HardTerm . Lit <$>
  (either (Int . fromIntegral) Double <$> naturalOrDouble
    <|> String <$> stringLiteral
    <|> Char <$> charLiteral)

term :: (Monad m, TokenParsing m) => m Tm
term = term2

terms :: (Monad m, TokenParsing m) => m [Tm]
terms = commaSep term
