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
import Data.Maybe (catMaybes, fromMaybe)
import Data.Monoid ((<>), mempty)
import Data.Text (Text, pack, unpack)
import Data.Traversable (forM)
import Ermine.Builtin.Term hiding (explicit)
import Ermine.Parser.Data (dataType)
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
moduleHead :: (Monad m, TokenParsing m) => m (ModuleHead Import Text)
moduleHead = ModuleHead <$> moduleDecl <*> imports <*> (pack <$> many anyChar)

-- | Parse the rest of the file, incorporating the argument.
--
-- Between 'moduleHead' and here, you have to grab the 'Text' to feed
-- to the resulting parser, and associate each 'Import' with its
-- 'Module'.  Modules can't import in cycles, so you should detect
-- this when parsing 'Module's in an import graph from 'moduleHead's.
wholeModule :: (MonadPlus m, TokenParsing m) =>
               ModuleHead (Import, Module) a -> m Module
wholeModule mh =
  evalStateT statements (importedFixities $ mh^.moduleHeadImports)
  >>= assembleModule (mh^.module_) (fst <$> mh^.moduleHeadImports)

moduleDecl :: (Monad m, TokenParsing m) => m ModuleName
moduleDecl = symbol "module" *> moduleIdentifier <* symbol "where"
             <?> "module header"

-- | Declare initial fixities based on imports.
importedFixities :: [(Import, Module)] -> [FixityDecl]
importedFixities imps = imps >>= uncurry f where
  f :: Import -> Module -> [FixityDecl]
  f (Import _ mn as scop) m = undefined

imports :: (Monad m, TokenParsing m) => m [Import]
imports = importExportStatement `sepEndBy` semi <?> "import statements"

importExportStatement :: (Monad m, TokenParsing m) => m Import
importExportStatement =
  imp <$> (Private <$ symbol "import" <|> Public <$ symbol "export")
  <*> do
    src <- moduleIdentifier
    (src,,) <$> optional (symbol "as" *> moduleIdentifierPart)
            <*> optional (((Using  <$ symbol "using") <|>
                           (Hiding <$ symbol "hiding"))  <*> impList src)
  <?> "import/export statement"
  where imp pop (mi, as, usingpExps) =
          Import pop mi as (fromMaybe (Hiding []) usingpExps)
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

-- | A shim to prove the decomposition of 'assembleBindings'.
-- Supposing I have an initial value, a step of either type or term,
-- and a finalization to emit remaining terms and check for errors, I
-- can reproduce 'assembleBindings'.
outOfOrderBindings :: Monad m =>
                      x
                      -> (Either (Privacy, [a], Ann) (Privacy, a, [PreBody Ann a])
                          -> x
                          -> m (Maybe (Privacy, Term.Binding Ann a), x))
                      -> (x -> m [(Privacy, Term.Binding Ann a)])
                      -> [(Privacy, [a], Ann)]
                      -> [(Privacy, a, [PreBody Ann a])]
                      -> m [(Privacy, Term.Binding Ann a)] -- ^ bindings
outOfOrderBindings init step final types terms =
  flip evalStateT init $ liftM2 (<>)
         (liftM catMaybes
          . mapM (StateT . step)
          $ fmap Left types <> fmap Right terms)
         (get >>= lift . final)

statements :: (MonadState s m, HasFixities s, TokenParsing m) =>
              m [Statement Text Text]
statements = ([] <$ eof) <|> ((:) <$> statement <*> (semi *> statements))

defaultPrivacyTODO :: Privacy
defaultPrivacyTODO = Public

statement :: (MonadState s m, HasFixities s, TokenParsing m) =>
             m (Statement Text Text)
statement = FixityDeclStmt <$> (fixityDecl >>= installFixityDecl)
        <|> DataTypeStmt defaultPrivacyTODO <$> dataType
        <|> ClassStmt <$> undefined <*> undefined
        <|> uncurry (SigStmt defaultPrivacyTODO) <$> sigs
        <|> uncurry (TermStmt defaultPrivacyTODO) <$> termStatement
  where installFixityDecl fd = fd <$ (fixityDecls %= (fd:))

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

termStatement :: (MonadState s m, HasFixities s, TokenParsing m) => m (Text, [PreBody Ann Text])
termStatement = do
  (name, headBody) <- termDeclClause termIdentifier
  many (termDeclClause (semi *> (name <$ symbol (unpack name))))
    <&> \tailBodies -> (name, headBody : map snd tailBodies)

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
fetchModuleGraphM :: (HasModuleHead mh imp txt, HasImport imp, Monad m)
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
              subs <- deps a `forM` \k' ->
                go k' (g HM.! k') lk' sk'
              let here = maximum (0:map (1+) subs)
              modify (HM.insert k here)
              return here)
        extractTopo :: HashMap k Int -> [[(k, a)]]
        extractTopo = fmap (fmap (\k -> (k, g HM.! k)) . snd)
                    . M.toAscList . invertHM

-- Invariant: every [k] is nonempty
invertHM :: (Ord v) => HashMap k v -> M.Map v [k]
invertHM = flip HM.foldrWithKey M.empty $ \k ->
             M.alter (Just . maybe [k] (k:))
