{-# LANGUAGE DeriveFoldable, DeriveFunctor, DeriveTraversable #-}
module Ermine.Core.Module
 ( Module(Module)
 , definitions
 , termExports
 , instances
 , types
 , dataDecls
 ) where

import Control.Applicative
import Control.Lens
import Data.Binary as Binary
import Data.Bytes.Serial
import Data.ByteString
import Data.Foldable
import Data.Map
import Data.Serialize as Serialize
import Data.Void
import Ermine.Syntax.Data
import Ermine.Syntax.Global
import Ermine.Syntax.ModuleName
import Ermine.Syntax.Name
import Ermine.Syntax.Type

data Module a = Module
  { _name         :: ModuleName
  , _dependencies :: [ModuleName]
  , _definitions  :: [a]
  , _termExports  :: Map Global (Either Global Int)
  , _instances    :: Map ByteString Int
  , _types        :: Map Global (Type Void Void)
  , _dataDecls    :: [DataType Void Void]
  } deriving (Eq,Show,Foldable,Functor,Traversable)

instance HasName (Module a) where
  name = moduleName.name

instance HasModuleName (Module a) where
  moduleName f m@Module{_name = nm} = f nm <&> \nm' -> m { _name = nm' }

dependencies :: Lens' (Module a) [ModuleName]
dependencies f (Module n deps defs ts is tys d) = f deps <&> \deps' -> Module n deps' defs ts is tys d

definitions :: Lens (Module a) (Module b) [a] [b]
definitions f (Module n deps defs ts is tys d) = f defs <&> \defs' -> Module n deps defs' ts is tys d

termExports :: Lens' (Module a) (Map Global (Either Global Int))
termExports f (Module n deps defs ts is tys d) = f ts <&> \ts' -> Module n deps defs ts' is tys d

instances :: Lens' (Module a) (Map ByteString Int)
instances f (Module n deps defs ts is tys d) = f is <&> \is' -> Module n deps defs ts is' tys d

types :: Lens' (Module a) (Map Global (Type Void Void))
types f (Module n deps defs ts is tys d) = f tys <&> \tys' -> Module n deps defs ts is tys' d

dataDecls :: Lens' (Module a) [DataType Void Void]
dataDecls f (Module n deps defs ts is tys d) = f d <&> \d' -> Module n deps defs ts is tys d'

instance Serial1 Module where
  serializeWith f m =
    serialize (m^.moduleName)      >>
    serialize (m^.dependencies) >>
    serializeWith f (m^.definitions)  >>
    serialize (m^.termExports)  >>
    serialize (m^.instances)    >>
    serialize (m^.types)        >>
    serialize (m^.dataDecls)
  deserializeWith g =
    Module <$> deserialize <*> deserialize <*> deserializeWith g <*> deserialize <*> deserialize <*> deserialize <*> deserialize

instance Serial a => Serial (Module a) where
  serialize = serializeWith serialize
  deserialize = deserializeWith deserialize

instance Binary a => Binary (Module a) where
  put = serializeWith Binary.put
  get = deserializeWith Binary.get

instance Serialize a => Serialize (Module a) where
  put = serializeWith Serialize.put
  get = deserializeWith Serialize.get
