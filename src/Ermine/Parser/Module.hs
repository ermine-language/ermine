{-# LANGUAGE TupleSections #-}
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
import Control.Monad (foldM)
import Data.List (intercalate)
import qualified Data.Map as M
import Data.Monoid (mempty)
import Data.Text (Text, unpack)
import Ermine.Builtin.Term hiding (explicit)
import Ermine.Parser.Data (dataType)
import Ermine.Parser.Style
import Ermine.Parser.Term
import Ermine.Parser.Type (Ann, annotation)
-- import Ermine.Syntax.Class
-- import Ermine.Syntax.Data
import Ermine.Syntax.Global (Fixity(..), Assoc(..))
import Ermine.Syntax.Module hiding (explicit)
import Ermine.Syntax.ModuleName
import qualified Ermine.Syntax.Term as Term
import Text.Parser.Combinators
import Text.Parser.Token

-- | Parser for a module.
wholeModule :: (Monad m, TokenParsing m) => m Module
wholeModule = do
  m <- moduleDecl
  i <- imports
  s <- statements
  assembleModule m i s

moduleDecl :: (Monad m, TokenParsing m) => m ModuleName
moduleDecl = symbol "module" *> moduleIdentifier <* symbol "where"
             <?> "module header"

imports :: (Monad m, TokenParsing m) => m [Import]
imports = importExportStatement `sepEndBy` semi <?> "import statements"

importExportStatement :: (Monad m, TokenParsing m) => m Import
importExportStatement =
  imp <$> (Private <$ symbol "import"
           <|> Public <$ symbol "export")
  <*> do
    src <- moduleIdentifier
    (src,,) <$> optional (symbol "as" *> moduleIdentifierPart)
            <*> optional ((,) <$> (False <$ symbol "using"
                                   <|> True <$ symbol "hiding")
                          <*> impList src)
  <?> "import/export statement"
  where imp pop (mi, as, usingpExps) =
          Import pop mi as (usingpExps ^. _Just._2)
                 (maybe False fst usingpExps)
        impList src = explicit src `sepEndBy` semi <?> "explicit imports"

moduleIdentifier :: (Monad m, TokenParsing m) => m ModuleName
moduleIdentifier = mkModuleName_ . intercalate "."
                   <$> moduleIdentifierPart `sepBy` dot
                   <?> "module name"

moduleIdentifierPart :: (Monad m, TokenParsing m) => m String
moduleIdentifierPart = ident (termCon & styleName .~ "module name")

explicit :: (Monad m, TokenParsing m) => ModuleName -> m Explicit
explicit fromModule = do
  isTy <- option False (True <$ symbol "type")
  let name = operator <|> (if isTy then ident typeCon
                           else (ident termCon <|> termIdentifier))
  flip Explicit isTy
    <$> (name >>= undefined fromModule)
    -- TODO ↑ look up Global conversion in fromModule parsestate
    -- TODO ↓ check 'as' name's fixity
    <*> optional (symbol "as" *> (unpack <$> name))

assembleModule :: (Monad m, TokenParsing m) =>
                  ModuleName
               -> [Import]
               -> [Statement Text Text]
               -> m Module
assembleModule nm im stmts =
  Module nm im (these _FixityDeclStmt) (these _DataTypeStmt)
           <$> assembleBindings (these _SigStmt) (these _TermStmt)
            ?? M.fromList (these _ClassStmt)
  where these p = stmts ^.. folded . p

-- TODO: We'll want/need to give location information on errors.
-- TODO: That may or may not require some extra info to be passed into this function.
-- TODO: We also might consider differentiating these errors:
--  multiple type annotations for: t
--  multiple bindings with name: t
assembleBindings :: (Monad m, TokenParsing m, Ord a, Show a) =>
                    [(Privacy, [a], Ann)]             -- ^ types
                 -> [(Privacy, a, [PreBody Ann a])]   -- ^ terms
                 -> m [(Privacy, Term.Binding Ann a)] -- ^ bindings
assembleBindings types terms = do
  types' <- foldM (\m (p, ts, a) -> foldM (\acc t -> insertGuarded t (p, a) acc) m ts) M.empty types
  terms' <- foldM (\m (privy, t, body) -> insertGuarded t (privy, body) m) M.empty terms
  assembleBindings' types' terms' where
    insertGuarded k _ m | M.member k m = fail   $ "multiple bindings with names(s): " ++ show k
    insertGuarded k a m | otherwise    = return $ M.insert k a m
    assembleBindings' types terms = foldM f [] (M.toList terms) where
      f acc (name, (p, ts)) = do
        x <- maybe (binding Term.Implicit) g $ M.lookup name types
        return $ x : acc where
        g (p', ann) | p == p'   = binding (Term.Explicit ann)
        g _         | otherwise = fail $ "found conflicting privacies for: " ++ show name
        binding bt = return (p, finalizeBinding [name] $ PreBinding mempty bt ts)

statements :: (Monad m, TokenParsing m) =>
              m [Statement Text Text]
statements = ([] <$ eof) <|> ((:) <$> statement <*> (semi *> statements))

defaultPrivacyTODO :: Privacy
defaultPrivacyTODO = Public

statement :: (Monad m, TokenParsing m) =>
             m (Statement Text Text)
statement = FixityDeclStmt <$> fixityDecl
        <|> DataTypeStmt defaultPrivacyTODO <$> dataType
        <|> ClassStmt <$> undefined <*> undefined
        <|> uncurry (SigStmt defaultPrivacyTODO) <$> sigs
        <|> uncurry (TermStmt defaultPrivacyTODO) <$> termStatement

fixityDecl :: (Monad m, TokenParsing m) => m FixityDecl
fixityDecl = FixityDecl
         <$> option False (True <$ symbol "type")
         <*> (fixity <*> prec)
         <*> many operator
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

sigs :: (Monad m, TokenParsing m) => m ([Text], Ann)
sigs = (,) <$> try (termIdentifier `sepBy` comma <* colon)
           <*> annotation

termStatement :: (Monad m, TokenParsing m) => m (Text, [PreBody Ann Text])
termStatement = do
  (name, headBody) <- termDeclClause termIdentifier
  many (termDeclClause (semi *> (name <$ symbol (unpack name))))
    <&> \tailBodies -> (name, headBody : map snd tailBodies)
