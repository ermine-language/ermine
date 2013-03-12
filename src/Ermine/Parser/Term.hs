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
import Control.Lens hiding (op)
import Control.Monad
import Data.List (elemIndex)
import Data.Traversable
import Ermine.Parser.Style
import Ermine.Parser.Type as Type
import Ermine.Parser.Pat
import Ermine.Syntax
import Ermine.Syntax.Prim
import Ermine.Syntax.Pat
import Ermine.Syntax.Term
import Ermine.Syntax.Kind (star)
import Ermine.Syntax.Type hiding (Var, App, Tuple)
import qualified Ermine.Syntax.Type as Type
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
 tup xs  = apps (HardTerm . Prim . Tuple $ length xs) xs

term1 :: (Monad m, TokenParsing m) => m Tm
term1 = foldl1 App <$> some term0

term2 :: (Monad m, TokenParsing m) => m Tm
term2 = lam <|> term1

bindings :: (Monad m, TokenParsing m) => m ([String], [Pat Ann])
bindings = do p@(vs, _) <- sequenceA <$> some pat0
              validate vs
              return p

lam :: (Monad m, TokenParsing m) => m Tm
lam = build <$> try (bindings <* reserve op "->") <*> term
 where
 build (vs, ps) body = Lam ps $ abstract (`elemIndex` vs) body

literal :: (Monad m, TokenParsing m) => m Tm
literal = HardTerm . Prim . either (Int . fromIntegral) Double <$> naturalOrDouble

term :: (Monad m, TokenParsing m) => m Tm
term = term2

terms :: (Monad m, TokenParsing m) => m [Tm]
terms = sepBy term comma
