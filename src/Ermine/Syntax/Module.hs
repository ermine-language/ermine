{-# LANGUAGE TemplateHaskell #-}

module Ermine.Syntax.Module
 ( Module(Module)
 , definitions
 , termExports
 , instances
 , types
 , dataDecls
 ) where

import Control.Applicative
import Control.Lens
import Data.Bytes.Serial
import Data.ByteString
import Data.Map
import Data.Void
import Ermine.Syntax.Core
import Ermine.Syntax.DataType
import Ermine.Syntax.Global
import Ermine.Syntax.Head
import Ermine.Syntax.ModuleName
import Ermine.Syntax.Name
import Ermine.Syntax.Type

data Module = Module {
  _name        :: ModuleName,
  _definitions :: [Core Int],
  _termExports :: Map Global (Either Global Int),
  _instances   :: Map ByteString Int,
  _types       :: Map Global (Type Void Void),
  _data        :: [DataType Void Void]
}

instance HasName Module where
  name = module_.name

instance HasModuleName Module where
  module_ f m@Module{_name = nm} = f nm <&> \nm' -> m { _name = nm' }

definitions :: Lens' Module [Core Int]
definitions f (Module n ds ts is tys d) = f ds <&> \ds' -> Module n ds' ts is tys d

termExports :: Lens' Module (Map Global (Either Global Int))
termExports f (Module n ds ts is tys d) = f ts <&> \ts' -> Module n ds ts' is tys d

instances :: Lens' Module (Map ByteString Int)
instances f (Module n ds ts is tys d) = f is <&> \is' -> Module n ds ts is' tys d

types :: Lens' Module (Map Global (Type Void Void))
types f (Module n ds ts is tys d) = f tys <&> \tys' -> Module n ds ts is tys' d

dataDecls :: Lens' Module [DataType Void Void]
dataDecls f (Module n ds ts is tys d) = f d <&> \d' -> Module n ds ts is tys d'

instance Serial Module where
  serialize m =
    serialize (m^.module_)     >>
    serialize (m^.definitions) >>
    serialize (m^.termExports) >>
    serialize (m^.instances)   >>
    serialize (m^.types)       >>
    serialize (m^.dataDecls)
  deserialize =
    Module <$> deserialize <*> deserialize <*> deserialize <*> deserialize <*> deserialize <*> deserialize