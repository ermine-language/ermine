
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

import Control.Lens
import Data.Functor
import Ermine.Inference.Kind
import Ermine.Syntax.ModuleName
import Ermine.Syntax.Module as Term
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
checkModule :: Term.Module -> M s (Core.Module a)
checkModule tm = do
  let (ps, dts) = unzip $ tm^.moduleData
  pdts <- zip ps <$> checkDataTypeKinds dts
  return $ Core.Module
    (tm^.moduleName)
    (tm^..moduleImports.resolvedImportList.traverse.importModule)
    undefined
    undefined
    undefined
    undefined
    (fmap snd . filter (has $ _1._Public) $ pdts)
