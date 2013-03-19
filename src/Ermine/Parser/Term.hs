{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternGuards #-}
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

import Control.Lens hiding (op)
import Control.Applicative
import Control.Comonad
import Control.Monad.State
import Data.Function
import Data.Either (partitionEithers)
import Data.List (groupBy, find)
import Data.Set as Set hiding (map)
import Data.Foldable (foldrM)
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

termDeclClause :: (Monad m, TokenParsing m) => m (String, Binder String [Pat Ann], Tm)
termDeclClause = (,,) <$> ident termIdent <*> pat0s <* reserve op "=" <*> term
 where
 pat0s = do ps <- sequenceA <$> many pat0
            ps <$ validate ps
                    (\n -> unexpected $ "duplicate bindings in pattern for: " ++ n)


declClauses :: (Monad m, TokenParsing m)
            => m [Either (String, Ann) (String, Binder String [Pat Ann], Tm)]
declClauses = semiSep $ (Left <$> typeDecl) <|> (Right <$> termDeclClause)

type TyDecl = (String, Ann)
type TmDecl = (String, [(Binder String [Pat Ann], Tm)])

decls :: (Monad m, TokenParsing m) => m ([TyDecl], [TmDecl])
decls = do (ts, cs) <- partitionEithers <$> declClauses
           fmap (ts,) . mapM validate $ groupBy ((==) `on` (^._1)) cs
 where
 tail3 (_, y, z) = (y, z)
 validate ((name, ps, b):rest)
   | all pl rest = return (name, (ps, b) : map tail3 rest)
   | otherwise   =
     fail $ "Equations for `" ++ name ++ "' have differing numbers of arguments."
  where pl (_, qs, _) = length (extract ps) == length (extract qs)

validateDecls :: (Monad m, TokenParsing m) => [TyDecl] -> [TmDecl] -> m ()
validateDecls tys tms
  | Just n <- findDuplicate tyns =
    fail $ "Duplicate type declarations for `" ++ n ++ "'."
  | Just n <- findDuplicate tmns =
    fail $ "Duplicate definitions for `" ++ n ++ "'."
  | Just n <- uncovered tmns tyns =
    fail $ "No definition for declared value `" ++ n ++ "'."
  | otherwise = return ()
 where
 tyns = map fst tys
 tmns = map fst tms

--bindings :: (Monad m, TokenParsing m) => m [Binding String String]
--bindings = do (tys, tms) <- decls
--              validateDecls tys tms
--              let tmns = map fst tms
-- where
-- mkbody ps b
--   | 

uncovered :: Ord a => [a] -> [a] -> Maybe a
uncovered xs ys = find (`Set.notMember` s) ys
 where
 s = Set.fromList xs

findDuplicate :: Ord a => [a] -> Maybe a
findDuplicate = flip evalState Set.empty . foldrM test Nothing
 where
 test e r = state $ \s -> if e `Set.member` s then (Just e, s) else (r, Set.insert e s)

