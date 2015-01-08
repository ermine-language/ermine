{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) McGraw Hill Financial 2014-2015
-- License   :  BSD2
-- Maintainer:  Stephen Compall <scompall@nocandysw.com>
-- Stability :  experimental
-- Portability: non-portable
--
-- State and helpers for loading modules in the REPL
--------------------------------------------------------------------

module Ermine.Console.Module
  ( moduleLoader
  ) where

import Control.Applicative
import Control.Lens
import Control.Monad (liftM)
import Control.Monad.Error.Class
-- import Control.Monad.IO.Class
import Control.Monad.State.Class
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Traversable (forM)
import Data.Foldable (foldl', forM_)
import Data.Monoid (mempty, (<>))
import Data.Text (Text, unpack)
import Ermine.Loader.Core (Loader, fanoutIdLoaderM, load,
                           thenM, thenM')
-- import Ermine.Loader.Filesystem (filesystemLoader, Freshness, LoadRefusal)
-- import Ermine.Loader.MapCache (loadCached)
import Ermine.Parser.Module
-- import Ermine.Parser.State
import Ermine.Pretty hiding (string, int, integer)
import Ermine.Syntax.Module (Import, Module, ModuleHead, importModule,
                             moduleName, moduleHeadImports, moduleHeadText)
import Ermine.Syntax.ModuleName (ModuleName)
import Ermine.Syntax.Name
import Text.Parser.Combinators (eof)
import Text.Trifecta.Parser
import Text.Trifecta.Result

-- | From a loader that loads the raw 'Text' of a module, produce a
-- 'Module' loader using the cache in state.
moduleLoader :: forall e m. (MonadError Doc m,
                             MonadState (HashMap ModuleName (e, Module)) m)
             => Loader e m ModuleName Text
             -> Loader e m ModuleName Module
moduleLoader l = fanoutIdLoaderM lemh `thenM'` afterModuleHead
  where lemh :: Loader e m ModuleName (ModuleHead Import Text)
        lemh = l `thenM` parseModuleHead
        dep :: ModuleName -> m (ModuleHead Import (e, Text))
        dep = liftM strength . (lemh^.load)
        importSet :: [ModuleHead Import (e, Text)] -> m ()
        importSet mhs = do
          preceding <- get
          -- Here we finally use the invariant of 'pickImportPlan':
          -- the 'Module's referenced by 'Import's *must* already be
          -- in the state.
          let precImp i = (i, snd $ preceding HM.! (i^.importModule))
          emods <- mhs `forM` \mh ->
              (mh^.moduleHeadText._1,)
              `liftM` parseModuleRemainder (bimap precImp snd mh)
          put (foldl' (\hm em@(_, md) -> HM.insert (md^.moduleName) em hm)
                      preceding emods)
        afterModuleHead (e, (n, mh)) = do
          already <- get
          impss <- pickImportPlan dep already n
                 . strength $ (e, mh)
          impss `forM_` importSet
          gets (HM.! n)

-- | Make a plan to import dependencies, based on importing a single
-- dependency.  Include all the module heads and remaining bodies in
-- the plan.
--
-- Invariant: For each element of the result, every 'Import'
-- referenced by each 'ModuleHead' appears in a previous element of
-- the result.
pickImportPlan :: MonadError Doc m
               => (ModuleName -> m (ModuleHead Import txt))
                  -- ^ load a module head by name
               -> HashMap ModuleName a -- ^ already-loaded Modules
               -> ModuleName           -- ^ initial module name
               -> ModuleHead Import txt -- ^ initial module head
               -> m [[ModuleHead Import txt]]
pickImportPlan dep hm n fstMH = do
  graph <- fetchGraphM deps (const . dep) (HM.singleton n fstMH)
  topSort graph deps &
    either (throwError . reportCircle) (return . fmap (fmap snd))
  where deps :: ModuleHead Import a -> [ModuleName]
        deps = filter (\mn -> not (HM.member mn hm))
             . toListOf (moduleHeadImports.folded.importModule)
        reportCircle circ = "Circular dependency detected: "
                         <> pretty (circ <&> pretty . unpack . (^.name))

-- Why isn't this defined?
strength :: Functor f => (a, f b) -> f (a, b)
strength (a, fb) = (a,) <$> fb

parseModuleHead :: MonadError Doc m => Text -> m (ModuleHead Import Text)
parseModuleHead = asMError . parseString (moduleHead <* eof) mempty . unpack

parseModuleRemainder :: MonadError Doc m
                     => ModuleHead (Import, Module) Text
                     -> m Module
parseModuleRemainder mh =
  mh ^. moduleHeadText
    & unpack
    & parseString (wholeModule mh <* eof) mempty
    & asMError

asMError :: MonadError Doc m => Result a -> m a
asMError (Success a) = return a
asMError (Failure doc) = throwError doc
