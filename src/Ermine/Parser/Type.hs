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
  , Typ
  , typ
  ) where

import Control.Applicative
import Control.Lens hiding (op)
-- import Data.HashSet as HashSet
import Data.Set as Set
import Data.Map as Map
import Data.Maybe
import Ermine.Builtin.Type
import Ermine.Parser.Keywords
import Ermine.Parser.Kind
import Ermine.Syntax
import Ermine.Syntax.Kind as Kind hiding (Var)
import Ermine.Syntax.Type
import Text.Parser.Combinators
import Text.Parser.Token
import Text.Parser.Token.Style

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
typ2 = chainr1 typ1 ((~>) <$ symbol "->")

typVarBinding :: (Monad m, TokenParsing m) => m ([String], Kind (Maybe String))
typVarBinding = flip (,) unk <$> some (ident tid)
            <|> parens ((,) <$> some (ident tid) <* colon <*> (fmap Just <$> kind))
 where
 unk = pure $ Nothing

typVarBindings :: (Monad m, TokenParsing m) => m [(String, Kind (Maybe String))]
typVarBindings = concatMap (\(vs, k) -> flip (,) k <$> vs) <$> some typVarBinding

forallBindings :: (Monad m, TokenParsing m) => m ([String], [(String, Kind (Maybe String))])
forallBindings = optional (braces (some (ident tid))) >>= \mks -> case mks of
  Just ks -> (,) ks . fromMaybe [] <$> optional typVarBindings
  Nothing -> (,) [] <$> typVarBindings

typ3 :: (Applicative m, Monad m, TokenParsing m) => m Typ
typ3 =  build <$ symbol "forall" <*> forallBindings <* dot <*> typ3
    <|> typ2
 where
 build (kvs, tvks) t = forall (Just <$> kvs) tvks (And []) t

-- | Parse a 'Type'.
typ :: (Monad m, TokenParsing m) => m Typ
typ = typ3

-- anyTyp = exists | typ
anyTyp :: (Monad m, TokenParsing m) => m Typ
anyTyp = typ
