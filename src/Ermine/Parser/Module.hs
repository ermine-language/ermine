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
import Data.List (intercalate)
import Data.Text (Text)
import Data.Void (Void)
import Ermine.Parser.Style (termCon)
import Ermine.Syntax.Data
import Ermine.Syntax.Global (Fixity(..), Assoc(..))
import Ermine.Syntax.Module
import Ermine.Syntax.ModuleName
import Ermine.Syntax.Term
import Ermine.Syntax.Type
import Text.Parser.Combinators
import Text.Parser.Token

-- | Parser for a module.
wholeModule :: (Monad m, TokenParsing m) => m Module
wholeModule = uncurry3
          <$> (Module <$> moduleDecl <*> imports)
          <*> definitions
  where uncurry3 f (a, b, c) = f a b c

moduleDecl :: (Monad m, TokenParsing m) => m ModuleName
moduleDecl = symbol "module" *> moduleIdentifier <* symbol "where"
             <?> "module header"

imports :: (Monad m, TokenParsing m) => m [Import]
imports = importExportStatement `sepEndBy` semi <?> "import statements"

importExportStatement :: (Monad m, TokenParsing m) => m Import
importExportStatement =
  imp <$> (Private <$ symbol "import"
           <|> Public <$ symbol "export")
  <*> moduleIdentifier
  <*> optional (symbol "as" *> moduleIdentifierPart)
  <*> optional ((,) <$> (False <$ symbol "using" <|> True <$ symbol "hiding")
                <*> impList)
  <?> "import/export statement"
  where imp pop mi as usingpExps =
          Import pop mi as (usingpExps ^. _Just._2)
                 (maybe False fst usingpExps)
        impList = undefined :: m [Explicit]

moduleIdentifier :: (Monad m, TokenParsing m) => m ModuleName
moduleIdentifier = mkModuleName_ . intercalate "."
                   <$> moduleIdentifierPart `sepBy` dot
                   <?> "module name"

moduleIdentifierPart :: (Monad m, TokenParsing m) => m String
moduleIdentifierPart = ident (termCon & styleName .~ "module name")

definitions :: (Monad m, TokenParsing m) =>
               m ([FixityDecl],
                  [(Privacy, DataType () Text)],
                  [(Privacy, Binding (Annot Void Text) Text)])
definitions = undefined

fixityDecl :: (Monad m, TokenParsing m) => m FixityDecl
fixityDecl = FixityDecl
         <$> option False (True <$ symbol "type")
         <*> (fixity <*> prec)
         <*> undefined
  where fixity = Infix L <$ symbol "infixl"
             <|> Infix R <$ symbol "infixr"
             <|> Infix N <$ symbol "infix"
             <|> Prefix <$ symbol "prefix"
             <|> Postfix <$ symbol "postfix"

prec :: (Monad m, TokenParsing m) => m Int
prec = (do n <- natural
           if n <= 10
             then return (fromInteger n)
             else empty)
       <?> "precedence between 0 and 10"
