{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
module Ermine.Syntax.Module where

import Control.Lens
import Data.Bytes.Serial
import Data.Binary
import Data.Map (Map)
import Data.Serialize
import Data.Data hiding (DataType)
import Data.Ix
import Data.Text
import Data.Void
import Ermine.Builtin.Term (PreBody())
import Ermine.Syntax.Class
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
  } deriving (Show, Eq, Data, Typeable)

makeClassy ''FixityDecl

{-
data InstanceDecl = InstanceDecl
  { _instanceDeclClass    :: Class
  , _instanceDeclDefaults :: ClassBinding
  } deriving (Show, Typeable)
-}

-- | Combine top-level statements into a single type.
data Statement t a = FixityDeclStmt FixityDecl
                   | DataTypeStmt Privacy (DataType () t)
                   | SigStmt Privacy [a] (Annot Void t)
                   | TermStmt Privacy a [PreBody (Annot Void t) a]
                   | ClassStmt a (Class () t)
  deriving (Show, Eq)

makeClassyPrisms ''Statement

data Module = Module
  { _moduleName      :: ModuleName
  , _moduleImports   :: [Import]
  , _moduleFixities  :: [FixityDecl]
  , _moduleData      :: [(Privacy, DataType () Text)] -- TODO: support type not just data
  , _moduleBindings  :: [(Privacy, Binding (Annot Void Text) Text)]
  , _moduleClasses   :: Map Text (Class () Text)
  -- , _moduleInstances :: Map Head () 
  } deriving (Typeable)

makeClassy ''Module

class HasFixities a where
  fixityDecls :: Lens' a [FixityDecl]

instance (a ~ FixityDecl) => HasFixities [a] where
  fixityDecls = id

instance HasFixities Module where
  fixityDecls = moduleFixities
