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

import Control.Lens ((??))
import Control.Applicative
import Data.Function
import Data.Either (partitionEithers)
import Data.List (groupBy)
import Data.Traversable hiding (mapM)
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

term1 :: (Monad m, TokenParsing m) => m Tm
term1 = match
    <|> foldl1 App <$> some term0

sig :: (Monad m, TokenParsing m) => m Tm
sig = (maybe id (Sig ??) ??) <$> term1 <*> optional (colon *> annotation)

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

typeDecl :: (Monad m, TokenParsing m) => m (String, Ann)
typeDecl = (,) <$> try (ident termIdent) <*> annotation

termDeclClause :: (Monad m, TokenParsing m) => m (String, [PP], Tm)
termDeclClause = (,,) <$> ident termIdent <*> many pat0 <* reserve op "=" <*> term

declClauses :: (Monad m, TokenParsing m) => m [Either (String, Ann) (String, [PP], Tm)]
declClauses = semiSep $ (Left <$> typeDecl) <|> (Right <$> termDeclClause)

decls :: (Monad m, TokenParsing m) => m ([(String, Ann)], [(String, [([PP], Tm)])])
decls = do (ts, cs) <- partitionEithers <$> declClauses
           fmap (ts,) . mapM validate $ groupBy ((==) `on` fst3) cs
 where
 fst3 (x, _, _) = x
 tail3 (_, y, z) = (y, z)
 validate ((name, ps, b):rest)
   | all pl rest = return (name, (ps, b) : map tail3 rest)
   | otherwise   =
     fail $ "Equations for `" ++ name ++ "' have differing numbers of arguments."
  where pl (_, qs, _) = length ps == length qs

