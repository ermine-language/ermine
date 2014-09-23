{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
module Ermine.Syntax.Module where

import Control.Lens
import Data.Bytes.Serial
import Data.Binary (Binary)
import Data.Serialize (Serialize)
import Data.Data
import Data.Ix
import Ermine.Syntax.ModuleName

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
  , _fixityDeclFixity :: Fixity
  , _fixityDeclNames  :: [Text]
  } deriving (Show,Typeable)

data Module = Module
  { _moduleName      :: ModuleName
  , _moduleImports   :: [Import]
  , _moduleFixities  :: [FixityDecl]
  , _moduleData      :: [(Privacy, DataType () Text)] -- TODO: support type not just data
  , _moduleBindings  :: [(Privacy, Binding (Annot Text) Text)]
  -- Annot isn't sufficient now
  -- , _moduleClasses   :: Map Text (Class, Binding (Annot (Either Text Int)) (Either Text Int))
  -- , _moduleInstances :: Map Head () 
  } deriving (Typeable)

makeClassy ''Module
