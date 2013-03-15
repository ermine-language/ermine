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

import Control.Applicative
import Data.Traversable
import Ermine.Builtin.Pat
import Ermine.Builtin.Term
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

branch :: (Monad m, TokenParsing m) => m (Alt Ann (Term Ann) String)
branch = do pp <- pat
            reserve op "->"
            body <- term
            validate pp $ \n -> unexpected $ "duplicate bindings in pattern for: " ++ n
            return $ alt pp body

match :: (Monad m, TokenParsing m) => m Tm
match = Case <$ symbol "case" <*> term <* symbol "of" <*> braces (semiSep branch)

term2 :: (Monad m, TokenParsing m) => m Tm
term2 = lambda <|> sig

bindings :: (Monad m, TokenParsing m) => m (Binder String [Pat Ann])
bindings = do pps <- sequenceA <$> some pat
              validate pps $ \n -> unexpected $ "duplicate bindings in pattern for: " ++ n
              return pps

lambda :: (Monad m, TokenParsing m) => m Tm
lambda = lam <$> try (bindings <* reserve op "->") <*> term

literal :: (Monad m, TokenParsing m) => m Tm
literal = HardTerm . Lit <$>
  (either (Int . fromIntegral) Double <$> naturalOrDouble
    <|> String <$> stringLiteral
    <|> Char <$> charLiteral)

term :: (Monad m, TokenParsing m) => m Tm
term = term2

terms :: (Monad m, TokenParsing m) => m [Tm]
terms = commaSep term
