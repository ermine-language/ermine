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
                          , declarations
                          , letBlock
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
            b <- term
            validate pp $ \n -> unexpected $ "duplicate bindings in pattern for: " ++ n
            return $ alt pp b

match :: (Monad m, TokenParsing m) => m Tm
match = Case <$ symbol "case" <*> term <* symbol "of" <*> braces (semiSep branch)

term2 :: (Monad m, TokenParsing m) => m Tm
term2 = lambda <|> sig

patterns :: (Monad m, TokenParsing m) => m (Binder String [Pat Ann])
patterns = do pps <- sequenceA <$> some pat
              validate pps $ \n -> unexpected $ "duplicate bindings in pattern for: " ++ n
              return pps

lambda :: (Monad m, TokenParsing m) => m Tm
lambda = lam <$> try (patterns <* reserve op "->") <*> term

literal :: (Monad m, TokenParsing m) => m Tm
literal = HardTerm . Lit <$>
  (either (Int . fromIntegral) Double <$> naturalOrDouble
    <|> String <$> stringLiteral
    <|> Char <$> charLiteral)

term :: (Monad m, TokenParsing m) => m Tm
term = letBlock <|> term2

letBlock :: (Monad m, TokenParsing m) => m Tm
letBlock = let_ <$ symbol "let" <*> braces declarations <* symbol "in" <*> term

terms :: (Monad m, TokenParsing m) => m [Tm]
terms = commaSep term

typeDecl :: (Monad m, TokenParsing m) => m TyDecl
typeDecl = (,) <$> try (ident termIdent <* colon) <*> annotation

termDeclClause :: (Monad m, TokenParsing m)
               => m (String, PBody)
termDeclClause =
    (,) <$> ident termIdent
        <*> (body <$> pat0s <* reserve op "=" <*> term <*> whereClause)
 where
 pat0s = do ps <- sequenceA <$> many pat0
            ps <$ validate ps
                    (\n -> unexpected $ "duplicate bindings in pattern for: " ++ n)

type PBody = PreBody Ann String
type Where = Binder String [Binding Ann String]

whereClause :: (Monad m, TokenParsing m) => m Where
whereClause = symbol "where" *> braces declarations <|> pure (pure [])

declClauses :: (Monad m, TokenParsing m) => m [Either TyDecl (String, PBody)]
declClauses = semiSep $ (Left <$> typeDecl) <|> (Right <$> termDeclClause)

type TyDecl = (String, Ann)
type TmDecl = (String, [PBody])

decls :: (Monad m, TokenParsing m) => m ([TyDecl], [TmDecl])
decls = do (ts, cs) <- partitionEithers <$> declClauses
           fmap (ts,) . mapM validateShape $ groupBy ((==) `on` (^._1)) cs
 where
 validateShape l = let (name:_, pbs) = unzip l in
   if shapely pbs
     then return (name, pbs)
     else fail $ "Equations for `" ++ name ++ "' have differing numbers of arguments."

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

declarations :: (Monad m, TokenParsing m) => m (Binder String [Binding Ann String])
declarations = do
  (tys, tms) <- decls
  let -- TODO: Rendering
      bindType s
        | Just t <- lookup s tys = explicit t
        | otherwise              = implicit
  bindings (extend (uncurry bindType) <$> tms) <$ validateDecls tys tms

uncovered :: Ord a => [a] -> [a] -> Maybe a
uncovered xs ys = find (`Set.notMember` s) ys
 where
 s = Set.fromList xs

findDuplicate :: Ord a => [a] -> Maybe a
findDuplicate = flip evalState Set.empty . foldrM test Nothing
 where
 test e r = state $ \s -> if e `Set.member` s then (Just e, s) else (r, Set.insert e s)

