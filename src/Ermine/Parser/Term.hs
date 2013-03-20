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

import Bound
import Control.Lens hiding (op)
import Control.Applicative
import Control.Comonad
import Control.Monad.State
import Data.Function
import Data.Either (partitionEithers)
import Data.List (groupBy, find, elemIndex)
import Data.Monoid
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
term = letBlock <|> term2

letBlock :: (Monad m, TokenParsing m) => m Tm
letBlock = let_ <$ symbol "let" <*> braces declarations <* symbol "in" <*> term

terms :: (Monad m, TokenParsing m) => m [Tm]
terms = commaSep term

typeDecl :: (Monad m, TokenParsing m) => m (String, Ann)
typeDecl = (,) <$> try (ident termIdent <* colon) <*> annotation

termDeclClause :: (Monad m, TokenParsing m)
               => m (String, Binder String [Pat Ann], Tm, Where)
termDeclClause =
    (,,,) <$> ident termIdent <*> pat0s <* reserve op "=" <*> term <*> whereClause
 where
 pat0s = do ps <- sequenceA <$> many pat0
            ps <$ validate ps
                    (\n -> unexpected $ "duplicate bindings in pattern for: " ++ n)

type Where = Binder String [Binding Ann String]

whereClause :: (Monad m, TokenParsing m) => m Where
whereClause = symbol "where" *> braces declarations <|> pure (pure [])

declClauses :: (Monad m, TokenParsing m)
            => m [Either (String, Ann) (String, Binder String [Pat Ann], Tm, Where)]
declClauses = semiSep $ (Left <$> typeDecl) <|> (Right <$> termDeclClause)

type TyDecl = (String, Ann)
type TmDecl = (String, [(Binder String [Pat Ann], Tm, Where)])

decls :: (Monad m, TokenParsing m) => m ([TyDecl], [TmDecl])
decls = do (ts, cs) <- partitionEithers <$> declClauses
           fmap (ts,) . mapM validateShape $ groupBy ((==) `on` (^._1)) cs
 where
 tail4 (_, x, y, z) = (x, y, z)
 validateShape [                  ] = error "decl:validateShape: IMPOSSIBLE"
 validateShape ((name, ps, b, wh):rest)
   | all pl rest = return (name, (ps, b, wh) : map tail4 rest)
   | otherwise   =
     fail $ "Equations for `" ++ name ++ "' have differing numbers of arguments."
  where pl (_, qs, _, _) = length (extract ps) == length (extract qs)

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
  let ns = map fst tms
      mkbody (Binder vs ps, b, wh) = Body ps (abstract f b) (fmap av <$> extract wh)
       where ws = vars wh
             av x | Just i <- elemIndex x vs = B i
                  | otherwise                = F x
             f x | Just i <- elemIndex x ws = Just (W i)
                 | Just i <- elemIndex x vs = Just (P i)
                 | Just i <- elemIndex x ns = Just (D i)
                 | otherwise                = Nothing
      -- TODO: Rendering
      mkbinding (s, pbs) = Binding mempty (bindType s) $ map mkbody pbs
      bindType s
        | Just t <- lookup s tys = Explicit t
        | otherwise              = Implicit
  Binder ns (map mkbinding tms) <$ validateDecls tys tms

uncovered :: Ord a => [a] -> [a] -> Maybe a
uncovered xs ys = find (`Set.notMember` s) ys
 where
 s = Set.fromList xs

findDuplicate :: Ord a => [a] -> Maybe a
findDuplicate = flip evalState Set.empty . foldrM test Nothing
 where
 test e r = state $ \s -> if e `Set.member` s then (Just e, s) else (r, Set.insert e s)

