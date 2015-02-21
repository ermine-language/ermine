{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
  ( -- * Two-phase module parsing
    moduleHead
  , wholeModule
    -- * Between phase 1 and 2
  , fetchGraphM
  , fetchModuleGraphM
  , topSort
  ) where

import Control.Applicative
import Control.Lens
import Control.Monad.State hiding (forM)
import Data.Hashable (Hashable)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import Data.List (intercalate)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Monoid ((<>), mempty, mconcat)
import Data.Text (Text, pack, unpack)
import Data.Traversable (for, forM)
import Ermine.Builtin.Term hiding (explicit)
import Ermine.Parser.Data (dataType)
import Ermine.Parser.Resolver
import Ermine.Parser.Style
import Ermine.Parser.Term
import Ermine.Parser.Type (Ann, annotation)
import Ermine.Syntax.Global (Fixity(..), Assoc(..))
import Ermine.Syntax.Module hiding (explicit, fixityDecl, moduleHead)
import Ermine.Syntax.ModuleName
import qualified Ermine.Syntax.Term as Term
import Text.Parser.Char
import Text.Parser.Combinators
import Text.Parser.Token

-- | Parse the whole file, but only the module head.
moduleHead :: (Monad m, TokenParsing m) => m (ModuleHead (Import Text) Text)
moduleHead = ModuleHead <$> moduleDecl <*> imports <*> (pack <$> many anyChar)

-- | Parse the rest of the file, incorporating the argument.
--
-- Between 'moduleHead' and here, you have to grab the 'Text' to feed
-- to the resulting parser, and associate each 'Import' with its
-- 'Module'.  Modules can't import in cycles, so you should detect
-- this when parsing 'Module's in an import graph from 'moduleHead's.
wholeModule :: (MonadPlus m, TokenParsing m) =>
               ModuleHead (Import Text, Module) a -> m Module
wholeModule mh = do
  resImps <- (mh^.moduleHeadImports) `forM` uncurry resolveImport
  stmts   <- evalStateT statements (importedFixities $ mh^.moduleHeadImports)
  assembleModule (mh^.moduleName) (mconcat resImps) stmts

moduleDecl :: (Monad m, TokenParsing m) => m ModuleName
moduleDecl = symbol "module" *> moduleIdentifier <* symbol "where"
             <?> "module header"

-- | Declare initial fixities based on imports.
importedFixities :: forall mod. HasFixities mod
                 => [(Import Text, mod)] -> [FixityDecl]
importedFixities imps = imps >>= uncurry f where
  f :: Import Text -> mod -> [FixityDecl]
  f imp md = md^.fixityDecls
               & case imp^.importScope of
                   Using [] -> const []
                   Hiding [] -> id
                   Using names -> useNames names
                   Hiding names -> dropNames names
               & maybe id renameNames (imp^.importAs)

useNames :: [Explicit Text] -> [FixityDecl] -> [FixityDecl]
useNames names =
  mapped.fixityDeclNames %~ mapMaybe (`HM.lookup` nameIndex)
  where nameIndex = HM.fromList (explicitName <$> names)
        -- TODO should this use the Global instead of Text?
        explicitName ex = let gn = ex^.explicitGlobal
                          in (gn, ex^.explicitLocal & maybe gn pack)

dropNames :: [Explicit Text] -> [FixityDecl] -> [FixityDecl]
dropNames names =
  mapped.fixityDeclNames %~ filter (not . (`HS.member` nameIndex))
  where nameIndex = HS.fromList . fmap (^.explicitGlobal) $ names

renameNames :: String -> [FixityDecl] -> [FixityDecl]
renameNames suffix =
  mapped.fixityDeclNames.mapped %~ (<> pack ("_" <> suffix))

imports :: (Monad m, TokenParsing m) => m [Import Text]
imports = bracedSemiSep importExportStatement <?> "import statements"

importExportStatement :: (Monad m, TokenParsing m) => m (Import Text)
importExportStatement =
  Import <$> (Private <$ symbol "import" <|> Public <$ symbol "export")
         <*> moduleIdentifier
         <*> optional (symbol "as" *> moduleIdentifierPart)
         <*> (fromMaybe (Hiding [])
              <$> optional (((Using  <$ symbol "using") <|>
                             (Hiding <$ symbol "hiding")) <*> impList))
  <?> "import/export statement"
  where impList = bracedSemiSep explicit <?> "explicit imports"

moduleIdentifier :: (Monad m, TokenParsing m) => m ModuleName
moduleIdentifier = mkModuleName_ . intercalate "."
                   <$> moduleIdentifierPart `sepBy` dot
                   <?> "module name"

moduleIdentifierPart :: (Monad m, TokenParsing m) => m String
moduleIdentifierPart = ident (termCon & styleName .~ "module name")

explicit :: (Monad m, TokenParsing m) => m (Explicit Text)
explicit = do
  isTy <- option False (True <$ symbol "type")
  let nam = operator <|> (if isTy then ident typeCon
                          else (ident termCon <|> termIdentifier))
  flip Explicit isTy
    <$> nam
    -- TODO ↓ check 'as' name's fixity
    <*> optional (symbol "as" *> (unpack <$> nam))

assembleModule :: (Monad m, TokenParsing m) =>
                  ModuleName
               -> ImportResolution
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
                 -> m [(Privacy, a, Term.Binding Ann a)] -- ^ bindings
assembleBindings typs terms = do
  typs' <- foldM (\m (p, ts, a) -> foldM (\acc t -> insertGuarded t (p, a) acc) m ts) M.empty typs
  terms' <- foldM (\m (privy, t, body) -> insertGuarded t (privy, body) m) M.empty terms
  assembleBindings' typs' terms' where
    insertGuarded k a m | M.member k m = fail   $ "multiple bindings with names(s): " ++ show k
                        | otherwise    = return $ M.insert k a m

assembleBindings' :: (Show k, Ord k, Monad m, Eq a) =>
                     M.Map k (a, t)
                  -> M.Map k (a, [PreBody t k])
                  -> m [(a, k, Term.Binding t k)]
assembleBindings' typs terms = foldM f [] termList where
  f acc (nam, (p, ts)) =
    (:acc) `liftM` maybe (binding Term.Implicit) g (M.lookup nam typs) where 
    g (p', ann) | p == p'   = binding (Term.Explicit ann)
                | otherwise = fail $ "found conflicting privacies for: " ++ show nam
    binding bt = return (p, nam, finalizeBinding termNms $ PreBinding mempty bt ts)
  termNms = fst <$> termList
  termList = M.toList terms

statements :: (MonadState s m, HasFixities s, TokenParsing m) =>
              m [Statement Text Text]
statements = bracedSemiSep statement

defaultPrivacyTODO :: Privacy
defaultPrivacyTODO = Public

statement :: (MonadState s m, HasFixities s, TokenParsing m) =>
             m (Statement Text Text)
statement = FixityDeclStmt <$> (fixityDecl >>= installFixityDecl)
        <|> DataTypeStmt defaultPrivacyTODO <$> dataType
        -- TODO when syntax found <|> ClassStmt <$> undefined <*> undefined
        <|> uncurry (SigStmt defaultPrivacyTODO) <$> sigs
        <|> uncurry (TermStmt defaultPrivacyTODO) <$> termStatement
  where installFixityDecl fd = fd <$ (fixityDecls %= (fd:))

fixityDecl :: (Monad m, TokenParsing m) => m FixityDecl
fixityDecl = FixityDecl
         <$> option TypeLevel (TermLevel <$ symbol "type")
         <*> (fixity <*> prec)
         <*> many operator
  where fixity = Infix L <$ symbol "infixl"
             <|> Infix R <$ symbol "infixr"
             <|> Infix N <$ symbol "infix"
             <|> Prefix  <$ symbol "prefix"
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

termStatement :: (MonadState s m, HasFixities s, TokenParsing m) => m (Text, [PreBody Ann Text])
termStatement = do
  (nam, headBody) <- termDeclClause termIdentifier
  many (termDeclClause (semi *> (nam <$ symbol (unpack nam))))
    <&> \tailBodies -> (nam, headBody : map snd tailBodies)

-- | Parse { ma; ... ma }
bracedSemiSep :: TokenParsing m => m a -> m [a]
bracedSemiSep = braces . semiSep

-- Module graph operations

-- | Given rules for identifying nodes and their dependencies, and
-- computing unique identifiers for each node, build up a dependency
-- set from an initial set.  Circularities are allowed.
fetchGraphM :: forall a k m. (Eq k, Hashable k, Monad m)
            => (a -> [k])         -- ^ Dependencies.
            -> (k -> a -> m a)    -- ^ Retrieve by identifier, requested by given node.
            -> HashMap k a     -- ^ Start the search.
            -> m (HashMap k a) -- ^ The final collection.
fetchGraphM deps fetch = join go where
  go :: HashMap k a -> HashMap k a -> m (HashMap k a)
  go consider found = if HM.null consider then return found else do
    consider' <- HM.foldl' (\a v -> do
      c <- a
      c' <- (HM.fromList (join (,) <$> deps v)
                `HM.difference` c `HM.difference` found)
             `forM` flip fetch v
      return (c `HM.union` c')) (return HM.empty) consider
    go consider' (found `HM.union` consider')

-- | A not particularly special specialization of 'fetchGraphM',
-- demonstrating its usage to recursively fetch dependency
-- 'ModuleHead's, given an action that parses them.
fetchModuleGraphM :: (HasModuleHead mh imp txt, HasImport imp g, Monad m)
                  => (ModuleName -> mh -> m mh)
                     -- ^ A 'ModuleHead' requests a 'ModuleHead' by name.
                  -> mh         -- ^ Initial 'ModuleHead'.
                  -> m (HashMap ModuleName mh)
fetchModuleGraphM retr mh =
  fetchGraphM (^.. moduleHeadImports.folded.importModule)
              retr
              (HM.singleton (mh^.moduleHeadName) mh)

-- | Group a dag into sets whose dependents are all to the right.
--
-- Invariant: Every element of the result is non-empty.
topSort :: forall a k. (Eq k, Hashable k)
        => HashMap k a       -- ^ The graph.
        -> (a -> [k])           -- ^ Dependencies.
        -> Either [k] [[(k, a)]]
           -- ^ The final grouping, those with no dependencies first,
           -- or the first detected cycle.
topSort g deps = fmap extractTopo
               . flip execStateT HM.empty
               . HM.foldrWithKey (\k v -> (go k v mempty mempty >>))
                                 (return ())
               $ g
  where go :: k -> a -> [k] -> HashSet k
           -> StateT (HashMap k Int) (Either [k]) Int
        go k a lk sk | HS.member k sk = lift (Left lk)
                     | otherwise = gets (HM.lookup k) >>=
          (flip maybe return $ do
              let lk' = k:lk
                  sk' = HS.insert k sk
              subs <- deps a `for` \k' ->
                go k' (g HM.! k') lk' sk'
              let here = 1 + maximum (-1:subs)
              modify (HM.insert k here)
              return here)
        extractTopo :: HashMap k Int -> [[(k, a)]]
        extractTopo = fmap (fmap (\k -> (k, g HM.! k)) . snd)
                    . M.toAscList . invertHM

-- Invariant: every [k] is nonempty
invertHM :: (Ord v) => HashMap k v -> M.Map v [k]
invertHM = flip HM.foldrWithKey M.empty $ \k ->
             M.alter (Just . maybe [k] (k:))
