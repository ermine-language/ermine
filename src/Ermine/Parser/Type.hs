--------------------------------------------------------------------
-- |
-- Module    :  Ermine.Parser.Type
-- Copyright :  (c) Edward Kmett and Dan Doel 2012
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
-- This module provides the parser for types
--------------------------------------------------------------------
module Ermine.Parser.Type
  ( tid
  , Ann
  , Typ
  , typ
  , annotation
  ) where

import Bound
import Control.Applicative
import Control.Lens hiding (op)
-- import Data.HashSet as HashSet
import Data.List (elemIndex)
import Data.Set as Set
import Data.Maybe
import Ermine.Builtin.Type
import Ermine.Parser.Keywords
import Ermine.Parser.Kind
import Ermine.Syntax
import Ermine.Syntax.Kind as Kind hiding (Var, constraint)
import Ermine.Syntax.Type
import Text.Parser.Combinators
import Text.Parser.Token
import Text.Parser.Token.Style

type Ann = Annot (Maybe String) String
type Typ = Type (Maybe String) String

-- | The internal token style used for type variables
tid :: TokenParsing m => IdentifierStyle m
tid = haskellIdents
  & styleName  .~ "type variable"
  & styleReserved .~ keywords

op :: TokenParsing m => IdentifierStyle m
op = haskellOps

banana :: (Monad m, TokenParsing m) => m a -> m a
banana p = reserve op "(|" *> p <* reserve op "|)"

-- | Parse an atomic type
typ0 :: (Monad m, TokenParsing m) => m Typ
typ0 = Var <$> ident tid
   <|> banana ( Var <$ reserve op ".." <*> ident tid
            <|> concreteRho . Set.fromList <$> sepBy (ident tid) comma -- TODO: check for collisions
              )
   <|> parens ( arrow <$ reserve op "->"
            <|> tuple . (+1) . length <$> some comma
            <|> tup <$> sepBy anyTyp comma
              )

typ1 :: (Monad m, TokenParsing m) => m Typ
typ1 = apps <$> typ0 <*> many typ0

-- TODO: typ2 shunting yard
typ2 :: (Monad m, TokenParsing m) => m Typ
typ2 = chainr1 typ1 ((~~>) <$ symbol "->")

-- | Parses an optionally annotated type variable.
--
--   typVarBinding ::= ( ident : kind )
--                   | ident
typVarBinding :: (Monad m, TokenParsing m) => m ([String], Kind (Maybe String))
typVarBinding = flip (,) unk <$> some (ident tid)
            <|> parens ((,) <$> some (ident tid) <* colon <*> (fmap Just <$> kind))
 where
 unk = pure $ Nothing

-- | Parses a series of type var bindings, processing the result to a more
-- usable format.
--
--   typVarBindings ::= typVarBinding typVarBindings0
--   typVarBindings0 ::= emtpy | typVarBinding typVarBindings0
typVarBindings :: (Monad m, TokenParsing m) => m [(String, Kind (Maybe String))]
typVarBindings = concatMap (\(vs, k) -> flip (,) k <$> vs) <$> some typVarBinding

-- | Parses the bound variables for a quantifier.
--
--   quantBindings ::= {kindVars}
--                   | typVarBindings
--                   | {kindVars} typVarBindings
quantBindings :: (Monad m, TokenParsing m) => m ([String], [(String, Kind (Maybe String))])
quantBindings = optional (braces (some (ident tid))) >>= \mks -> case mks of
  Just ks -> (,) ks . fromMaybe [] <$> optional typVarBindings
  Nothing -> (,) [] <$> typVarBindings

-- | Parser for a context that expects 0 or more constraints, together
--
--   constraints ::= constraints1
--                 | empty
--   constraints1 ::= constraint , constraints1
--                  | constraint
constraints :: (Monad m, TokenParsing m) => m Typ
constraints = allConstraints <$> sepBy constraint comma

-- | Parser for a context that expects a single constraint.
--   constraint ::= exists <vs>. constraint
--                | ( constraints )
--                | ident
constraint :: (Monad m, TokenParsing m) => m Typ
constraint =
      buildE <$ symbol "exists" <*> quantBindings <* dot <*> constraint
  <|> parens constraints
  -- Single constraints
  <|> Var <$> ident tid
 where buildE (kvs, tvks) body = exists (Just <$> kvs) tvks body

-- | Parses an optional constraint context, followed by an arrow if necessary.
--   constraint ::= constraint =>
--                | empty
constrained :: (Monad m, TokenParsing m) => m Typ
constrained = optional constraint >>= \mcs -> case mcs of
  Just cs -> cs <$ reserve op "=>"
  Nothing -> return $ And []

typ3 :: (Applicative m, Monad m, TokenParsing m) => m Typ
typ3 = forall [] [] <$> try constrained <*> typ4
    <|> typ2

typ4 :: (Applicative m, Monad m, TokenParsing m) => m Typ
typ4 =  build <$ symbol "forall" <*> quantBindings <* dot <*> typ4
    <|> typ3
 where
 build (kvs, tvks) t = forall (Just <$> kvs) tvks (And []) t

-- | Parse a 'Type'.
typ :: (Monad m, TokenParsing m) => m Typ
typ = typ4

-- anyTyp = exists | typ
anyTyp :: (Monad m, TokenParsing m) => m Typ
anyTyp = typ

annotation :: (Monad m, TokenParsing m) => m Ann
annotation = build <$> optional (symbol "some" *> some (ident tid) <* dot) <*> typ
 where
 build Nothing   t = annot t
 build (Just vs) t = Annot (length vs) (abstract (`elemIndex` vs) t)
