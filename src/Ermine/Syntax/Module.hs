{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
module Ermine.Syntax.Module where

import Control.Lens
import Data.Bytes.Serial
import Data.Binary
import Data.Serialize
import Data.Data hiding (DataType)
import Data.Ix
import Data.Text
import Data.Void
import Ermine.Syntax.Data
import Ermine.Syntax.Global as Global
import Ermine.Syntax.ModuleName
import Ermine.Syntax.Term
import Ermine.Syntax.Type
import GHC.Generics hiding (moduleName)

data Privacy = Private | Public deriving (Eq,Ord,Show,Read,Enum,Bounded,Ix,Generic,Typeable,Data)

instance Serial Privacy

instance Binary Privacy where
  put = serialize
  get = deserialize

instance Serialize Privacy where
  put = serialize
  get = deserialize

data Explicit = Explicit
  { _explicitGlobal :: Global
  , _explicitIsType :: Bool
  , _explicitLocal  :: Maybe String
  } deriving (Eq,Ord,Show,Read,Typeable)

makeClassy ''Explicit

data Import = Import -- TODO: add a location
  { _importPrivacy   :: Privacy
  , _importModule    :: ModuleName
  , _importAs        :: Maybe String
  , _importExplicits :: [Explicit]
  , _importUsing     :: Bool
  } deriving (Eq,Ord,Show,Read,Typeable)

makeClassy ''Import

data FixityDecl = FixityDecl
  { _fixityDeclType   :: Bool
  , _fixityDeclFixity :: Global.Fixity
  , _fixityDeclNames  :: [Text]
  } deriving (Show,Typeable)

data Module = Module
  { _moduleName      :: ModuleName
  , _moduleImports   :: [Import]
  , _moduleFixities  :: [FixityDecl]
  , _moduleData      :: [(Privacy, DataType () Text)] -- TODO: support type not just data
  , _moduleBindings  :: [(Privacy, Binding (Annot Void Text) Text)]
  -- Annot isn't sufficient now
  -- , _moduleClasses   :: Map Text (Class, Binding (Annot (Either Text Int)) (Either Text Int))
  -- , _moduleInstances :: Map Head () 
  } deriving (Typeable)

makeClassy ''Module
