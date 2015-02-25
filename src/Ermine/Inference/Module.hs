{-# LANGUAGE FlexibleContexts #-}

--------------------------------------------------------------------
-- |
-- Copyright :  (c) Dan Doel 2015
-- License   :  BSD2
-- Maintainer:  Dan Doel <dan.doel@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------

module Ermine.Inference.Module
  ( checkModule
  ) where

import Control.Applicative
import Control.Lens
import qualified Data.Map as Map
import Data.Bifunctor
import Data.Text (Text)
import Data.Void
import Ermine.Constraint.Env
import Ermine.Syntax.Data as Data
import Ermine.Inference.Kind as Kind
import Ermine.Inference.Type as Type
import Ermine.Syntax.Kind as Kind
import Ermine.Syntax.Global as Global
import Ermine.Syntax.ModuleName
import Ermine.Syntax.Module as Term
import Ermine.Syntax.Name
import Ermine.Syntax.Scope
import Ermine.Syntax.Type as Type
import Ermine.Core.Module as Core
import Ermine.Unification.Meta

-- data Module = Module
--   { mn               :: ModuleName
--   , _moduleImports   :: [Import Global]
--   , _moduleFixities  :: [FixityDecl]
--   , _moduleData      :: [(Privacy, DataType () Text)] -- TODO: support type not just data
--   , _moduleBindings  :: [(Privacy, Text, Binding (Annot Void Text) Text)]
--   , _moduleClasses   :: Map Text (Class () Text)
--   -- , _moduleInstances :: Map Head () 
--   } deriving (Show, Typeable)
--
--
-- data Module a = Module
--   { _name         :: ModuleName
--   , _dependencies :: [ModuleName]
--   , _definitions  :: [a]
--   , _termExports  :: Map Global (Either Global Int)
--   , _instances    :: Map ByteString Int
--   , _types        :: Map Global (Type Void Void)
--   , _dataDecls    :: [DataType Void Void]
--   } deriving (Eq,Show,Foldable,Functor,Traversable)
checkModule
  :: MonadConstraint (KindM s) s m
  => Term.Module
  -> (Text -> Global) -- Type map
  -> (Text -> Global) -- Term map
  -> (Global -> Schema Void)
  -> (Global -> Type Void Void)
  -> m (Core.Module a)
checkModule tm resolveType resolveTerm globalKind globalType = do
  (pdts, resolveData) <- let
      (ps, dts) = unzip $ tm^.moduleData
    in checkDataTypeKinds dts >>= \cdts ->
       pure ( zip ps cdts
            , Map.fromList $ cdts <&> \dt -> (dt^.name, dataTypeCon dt)
            )
  let resolveType' nm = case Map.lookup nm resolveData of
        Just c  -> hardType # c
        Nothing -> con g (globalKind g) where g = resolveType nm
  ptms <- let ps = view _1 <$> tm^.moduleBindings
              ns = view _2 <$> tm^.moduleBindings
              bs = view _3 <$> tm^.moduleBindings
           in inferBindings 0 (bimap absurd absurd . globalType) $
                bimap (first absurd . boundBy resolveType') resolveTerm <$> bs
  return $ Core.Module
    (tm^.moduleName)
    (tm^..moduleImports.resolvedImports.traverse.importModule)
    undefined
    undefined
    undefined
    undefined
    (fmap snd . filter (has $ _1._Public) $ pdts)
