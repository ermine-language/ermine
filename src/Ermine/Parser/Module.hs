{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) McGraw Hill Financial 2014
-- License   :  BSD2
-- Maintainer:  Stephen Compall <scompall@nocandysw.com>
-- Stability :  experimental
-- Portability: non-portable
--
-- This module provides the parser for modules
--------------------------------------------------------------------

module Ermine.Parser.Module
  ( wholeModule
  ) where

import Control.Applicative
import Control.Lens
import Data.Text (Text)
import Data.Void (Void)
import Ermine.Syntax.Data
import Ermine.Syntax.Module
import Ermine.Syntax.ModuleName
import Ermine.Syntax.Term
import Ermine.Syntax.Type
import Text.Parser.Char (alphaNum, char, upper)
import Text.Parser.Combinators
import Text.Parser.Token

-- | Parser for a module.
wholeModule :: (Monad m, TokenParsing m) => m Module
wholeModule = uncurry3
          <$> (Module <$> moduleDecl <*> imports)
          <*> definitions
  where uncurry3 f (a, b, c) = f a b c

moduleDecl :: TokenParsing m => m ModuleName
moduleDecl = symbol "module" *> moduleIdentifier <* symbol "where"

imports :: TokenParsing m => m [Import]
imports = many $ imp <$> (Private <$ symbol "import"
                          <|> Public <$ symbol "export")
          <*> option False (True <$ symbol "qualified")
          <*> moduleIdentifier
          <*> (Left <$> (symbol "as" *> moduleIdentifierPart)
               <|> Right <$> optional (parens impList))
  where imp pop qual mi asOrList =
          Import pop mi (asOrList ^? _Left)
                 (asOrList ^.. _Right._Just.folded)
                 undefined
        impList = undefined :: m [Explicit]

moduleIdentifier :: TokenParsing m => m ModuleName
moduleIdentifier = mkModuleName_ . concat <$> moduleIdentifierPart `sepBy` dot

moduleIdentifierPart :: TokenParsing m => m String
moduleIdentifierPart = (:) <$> upper <*> many (alphaNum <|> char '_')

definitions :: (Monad m, TokenParsing m) =>
               m ([FixityDecl],
                  [(Privacy, DataType () Text)],
                  [(Privacy, Binding (Annot Void Text) Text)])
definitions = undefined
