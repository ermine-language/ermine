{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Ermine.Syntax.ModuleName
  ( ModuleName(ModuleName)
  , mkModuleName
  , mkModuleName_
  , HasModuleName(..)
  ) where

import Control.Applicative
import Control.Lens
import Crypto.Classes hiding (hash)
import Crypto.Hash.MD5 as MD5
import Data.Binary
import Data.Bytes.Serial
import Data.ByteString
import Data.Data
import Data.Function
import Data.Hashable
import Data.Serialize
import Data.Text
import Ermine.Syntax.Digest
import Ermine.Syntax.Name
import GHC.Generics

data ModuleName = ModuleName
  { _digest   :: !ByteString
  , _package  :: !Text
  , _name     :: !Text
  } deriving (Data, Typeable, Generic)



mkModuleName :: Text -> Text -> ModuleName
mkModuleName p m = ModuleName d p m where
  d = MD5.finalize $ digest initialCtx p `digest` m

mkModuleName_ :: String -> ModuleName
mkModuleName_ nam = mkModuleName (Data.Text.pack "ermine") (Data.Text.pack nam)

instance Show ModuleName where
  showsPrec d (ModuleName _ p n) = showParen (d > 10) $
    showString "mkModuleName " . showsPrec 11 p .
                  showChar ' ' . showsPrec 11 n

instance Read ModuleName where
  readsPrec d = readParen (d > 10) $ \r -> do
    ("mkModuleName", r') <- lex r
    (p, r'') <- readsPrec 11 r'
    (n, r''')  <- readsPrec 11 r''
    return (mkModuleName p n, r''')

instance Eq ModuleName where
  (==) = (==) `on` _digest

instance Ord ModuleName where
  compare = compare `on` _digest

instance Hashable ModuleName where
  hashWithSalt s c = hashWithSalt s (_digest c)

instance HasName ModuleName
  where name f (ModuleName _ pkg nm) = mkModuleName pkg <$> f nm

class HasModuleName t where
  module_         :: Lens' t ModuleName

  package     :: Lens' t Text
  package f = module_ $ \(ModuleName _ pkg nm) -> f pkg <&> \pkg' -> mkModuleName pkg' nm

instance HasModuleName ModuleName where module_ = id

instance Digestable ModuleName where
  digest c ModuleName{_digest = d} = updateCtx c d

instance Serial ModuleName where
  serialize mn = serialize (_digest mn) >> serialize (mn^.package) >> serialize (mn^.name)
  deserialize = ModuleName <$> deserialize <*> deserialize <*> deserialize

instance Binary ModuleName where
  get = deserialize ; put = serialize

instance Serialize ModuleName where
  get = deserialize; put = serialize
