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

import Bound
import Control.Applicative
import Control.Lens hiding (op)
-- import Data.HashSet as HashSet
import Data.Set as Set
import Data.List as List
import Ermine.Builtin.Type
import Ermine.Parser.Keywords
import Ermine.Syntax
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

typ3 :: (Applicative m, Monad m, TokenParsing m) => m Typ
typ3 =  build <$ symbol "forall" <*> some (ident tid) <* dot <*> typ2
    <|> typ2
 where
 build vs t =
   Forall 0
          (List.map (const unk) vs)
          []
          (abstract (`elemIndex` vs) (abstractKinds (const Nothing) t))
 unk = Scope . pure . F . pure $ Nothing

-- | Parse a 'Type'.
typ :: (Monad m, TokenParsing m) => m Typ
typ = typ3

-- anyTyp = exists | typ
anyTyp :: (Monad m, TokenParsing m) => m Typ
anyTyp = typ
