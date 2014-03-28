--------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett and Dan Doel 2012
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
-- This module provides the parser for types
--------------------------------------------------------------------
module Ermine.Parser.Type
  ( Ann
  , Typ
  , typ
  , typ0
  , annotation
  , typVarBinding
  , typVarBindings
  , someTypVarBindings
  , quantifier
  , quantBindings
  ) where

import Bound
import Control.Applicative
import Data.List (elemIndex)
import Data.Text (Text)
import Ermine.Builtin.Type
import Ermine.Parser.Kind
import Ermine.Parser.Style
import Ermine.Syntax
import Ermine.Syntax.Hint
import Ermine.Syntax.Kind as Kind hiding (Var, constraint)
import Ermine.Syntax.Type
import Text.Parser.Combinators
import Text.Parser.Token

type Ann = Annot (Maybe Text) Text
type Typ = Type (Maybe Text) Text

banana :: (Monad m, TokenParsing m) => m a -> m a
banana p = reserve op "(|" *> p <* reserve op "|)"

-- | Parse an atomic type
typ0 :: (Monad m, TokenParsing m) => m Typ
typ0 = Var <$> typeIdentifier
   <|> banana ( Var <$ reserve op ".." <*> typeIdentifier
            <|> concreteRho <$> commaSep typeIdentifier
              )
   <|> parens ( arrow <$ reserve op "->"
            <|> tuple . (+1) . length <$> some comma
            <|> tup' <$> commaSep anyTyp
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
typVarBinding :: (Monad m, TokenParsing m) => m ([Text], Kind (Maybe Text))
typVarBinding = flip (,) unk <$> some typeIdentifier
            <|> parens ((,) <$> some typeIdentifier <* colon <*> (fmap Just <$> kind))
 where
 unk = pure Nothing

-- | Parses a series of type var bindings, processing the result to a more
-- usable format.
--
--   typVarBindings ::= emtpy | someTypVarBindings
typVarBindings :: (Monad m, TokenParsing m) => m [(Text, Kind (Maybe Text))]
typVarBindings = concatMap (\(vs, k) -> flip (,) k <$> vs) <$> many typVarBinding

-- | Parses a series of type var bindings, processing the result to a more
-- usable format.
--
--   someTypVarBindings ::= typVarBinding typVarBindings
someTypVarBindings :: (Monad m, TokenParsing m) => m [(Text, Kind (Maybe Text))]
someTypVarBindings = concatMap (\(vs, k) -> flip (,) k <$> vs) <$> some typVarBinding

-- | Parses the bound variables for a quantifier.
--
--   quantBindings ::= {kindVars}
--                   | someTypVarBindings
--                   | {kindVars} typVarBindings
quantBindings :: (Monad m, TokenParsing m) => m ([Text], [(Text, Kind (Maybe Text))])
quantBindings = optional (braces $ some typeIdentifier) >>= \mks -> case mks of
  Just ks -> (,) ks <$> typVarBindings
  Nothing -> (,) [] <$> someTypVarBindings

-- | Parses a quantifier.
--
--   quantifier ::= Q quantBindings .
quantifier :: (Monad m, TokenParsing m)
           => String -> m ([Text], [(Text, Kind (Maybe Text))])
quantifier q = symbol q *> quantBindings <* dot

-- | Parser for a context that expects 0 or more constraints, together
--
--   constraints ::= constraints1
--                 | empty
--   constraints1 ::= constraint , constraints1
--                  | constraint
constraints :: (Monad m, TokenParsing m) => m Typ
constraints = allConstraints <$> commaSep constraint

-- | Parser for a context that expects a single constraint.
--   constraint ::= exists <vs>. constraint
--                | ( constraints )
--                | ident
constraint :: (Monad m, TokenParsing m) => m Typ
constraint =
      buildE <$ symbol "exists" <*> quantBindings <* dot <*> constraint
  <|> parens constraints
  <|> apps lame <$ symbol "Lame" <*> many typ0
  -- Single constraints
  <|> Var <$> typeIdentifier
 where buildE (kvs, tvks) =
         exists maybeHint stringHint (Just <$> kvs) tvks

typ3 :: (Applicative m, Monad m, TokenParsing m) => m Typ
typ3 = forall maybeHint stringHint [] [] <$> try (constraint <* reserve op "=>") <*> typ4
    <|> typ2

typ4 :: (Applicative m, Monad m, TokenParsing m) => m Typ
typ4 =  build <$ symbol "forall" <*> quantBindings <* dot <*> typ4
    <|> typ3
 where
 build (kvs, tvks) = forall maybeHint stringHint (Just <$> kvs) tvks (And [])

-- | Parse a 'Type'.
typ :: (Monad m, TokenParsing m) => m Typ
typ = typ4

-- anyTyp = exists | typ
anyTyp :: (Monad m, TokenParsing m) => m Typ
anyTyp = typ

annotation :: (Monad m, TokenParsing m) => m Ann
annotation = build <$> optional (symbol "some" *> someTypVarBindings <* dot) <*> typ
 where
 build Nothing    t = annot t
 build (Just vks) t = Annot ks (abstract (`elemIndex` vs) t)
  where (vs, ks) = unzip vks
