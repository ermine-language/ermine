{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) Dan Doel 2012
-- License   :  BSD3
-- Maintainer:  Dan Doel <dan.doel@gmail.com>
-- Stability :  experimental
-- Portability: non-portable (DeriveDataTypeable)
--
-- This module provides the AST for data type declarations
--------------------------------------------------------------------

module Ermine.Syntax.DataType
  ( Constructor(Constructor)
  , ekinds
  , etypes
  , fields
  , DataType(DataType)
  , kparams
  , tparams
  , typeParameters
  , constrs
  , constructors
  , dataTypeSchema
  ) where

import Bound
import Bound.Scope
import Control.Applicative
import Control.Comonad
import Control.Lens
import Data.Bifoldable
import Data.Bitraversable
import Data.Binary as Binary
import Data.Bytes.Serial
import Data.Foldable (Foldable)
import Data.Serialize as Serialize
import Data.Typeable
import Ermine.Syntax
import Ermine.Syntax.Hint
import Ermine.Syntax.Global
import Ermine.Syntax.Kind as Kind
import Ermine.Syntax.Scope
import Ermine.Syntax.Type as Type
import GHC.Generics hiding (Constructor)
import Prelude.Extras

data Constructor k t =
  Constructor { _cname  :: Global
              , _ekinds :: [Hint]
              , _etypes :: [Hinted (Scope Int Kind k)]
              , _fields :: [Scope Int (TK k) t]
              }
 deriving (Show, Eq, Functor, Foldable, Traversable, Typeable, Generic)

instance HasGlobal (Constructor k t) where
  global = lens _cname (\c t -> c { _cname = t })

ekinds :: Lens' (Constructor k t) [Hint]
ekinds = lens _ekinds (\c ks -> c { _ekinds = ks })

etypes :: Lens' (Constructor k t) [Hinted (Scope Int Kind k)]
etypes = lens _etypes (\c ts -> c { _etypes = ts })

fields :: Lens' (Constructor k t) [Scope Int (TK k) t]
fields = lens _fields (\c fs -> c { _fields = fs })

instance Show k => Show1 (Constructor k)
instance Show2 Constructor

instance Eq k => Eq1 (Constructor k)
instance Eq2 Constructor

instance Bifunctor Constructor where
  bimap = bimapDefault

instance Bifoldable Constructor where
  bifoldMap = bifoldMapDefault

instance Bitraversable Constructor where
  bitraverse f g (Constructor n ek et as) =
    Constructor n ek
      <$> traverse (traverse (traverseScope pure f)) et
      <*> traverse (bitraverseScope (traverse f) g) as

instance HasKindVars (Constructor k t) (Constructor k' t) k k' where
  kindVars f = bitraverse f pure

instance HasTypeVars (Constructor k t) (Constructor k t') t t' where
  typeVars = traverse

data DataType k t =
  DataType { _dtname  :: Global
           , _kparams :: [Hint]
           , _tparams :: [Hinted (Scope Int Kind k)]
           , _constrs :: [Constructor (Var Int k) (Var Int t)]
           }
  deriving (Show, Eq, Foldable, Traversable, Functor, Typeable, Generic)

instance HasGlobal (DataType k t) where
  global = lens _dtname (\dt g -> dt { _dtname = g })

kparams :: Lens' (DataType k t) [Hint]
kparams = lens _kparams (\dt ks -> dt { _kparams = ks })

tparams :: Lens' (DataType k t) [Hinted (Scope Int Kind k)]
tparams = lens _tparams (\dt ks -> dt { _tparams = ks })

typeParameters :: Traversal' (DataType k t) (Hinted (Scope Int Kind k))
typeParameters = tparams.traverse

constrs :: Lens' (DataType k t) [Constructor (Var Int k) (Var Int t)]
constrs = lens _constrs (\dt ks -> dt { _constrs = ks })

constructors :: Traversal' (DataType k t) (Constructor (Var Int k) (Var Int t))
constructors = constrs.traverse

dataTypeSchema :: DataType k t -> Schema k
dataTypeSchema dt =
  Schema (dt^.kparams) (Scope . foldr (~>) star $ unscope . extract <$> dt^.tparams)

instance Show k => Show1 (DataType k)
instance Show2 DataType

instance Eq k => Eq1 (DataType k)
instance Eq2 DataType

instance Bifunctor DataType where bimap = bimapDefault
instance Bifoldable DataType where bifoldMap = bifoldMapDefault

instance Bitraversable DataType where
  bitraverse f g (DataType nm ks ts cs) =
    DataType nm ks
      <$> traverse (traverse $ traverseScope pure f) ts
      <*> traverse (bitraverse (traverse f) (traverse g)) cs

instance HasKindVars (DataType k t) (DataType k' t) k k' where
  kindVars f = bitraverse f pure

instance HasTypeVars (DataType k t) (DataType k t') t t' where
  typeVars = traverse

-- Serialization
instance Serial2 Constructor where
  serializeWith2 pk pt (Constructor nm ek et as) =
    serialize nm *>
    serialize ek *>
    serializeWith (serializeWith $ serializeScope3 serialize serializeWith pk) et *>
    serializeWith (serializeScope3 serialize (serializeTK pk) pt) as

  deserializeWith2 gk gt =
    Constructor
      <$> deserialize
      <*> deserialize
      <*> deserializeWith
            (deserializeWith $ deserializeScope3 deserialize deserializeWith gk)
      <*> deserializeWith (deserializeScope3 deserialize (deserializeTK gk) gt)

instance Serial k => Serial1 (Constructor k) where
  serializeWith = serializeWith2 serialize
  deserializeWith = deserializeWith2 deserialize

instance (Serial k, Serial t) => Serial (Constructor k t) where
  serialize = serializeWith serialize
  deserialize = deserializeWith deserialize

instance (Binary k, Binary t) => Binary (Constructor k t) where
  get = deserializeWith2 Binary.get Binary.get
  put = serializeWith2 Binary.put Binary.put

instance Serial2 DataType where
  serializeWith2 pk pt (DataType nm ks ts cs) =
    serialize nm *>
    serialize ks *>
    serializeWith (serializeWith $ serializeScope3 serialize serializeWith pk) ts *>
    serializeWith (serializeWith2 (serializeWith pk) (serializeWith pt)) cs

  deserializeWith2 gk gt =
    DataType
      <$> deserialize
      <*> deserialize
      <*> deserializeWith
            (deserializeWith $ deserializeScope3 deserialize deserializeWith gk)
      <*> deserializeWith (deserializeWith2 (deserializeWith gk) (deserializeWith gt))

instance Serial k => Serial1 (DataType k) where
  serializeWith = serializeWith2 serialize
  deserializeWith = deserializeWith2 deserialize

instance (Serial k, Serial t) => Serial (DataType k t) where
  serialize = serializeWith serialize
  deserialize = deserializeWith deserialize

instance (Binary k, Binary t) => Binary (DataType k t) where
  get = deserializeWith2 Binary.get Binary.get
  put = serializeWith2 Binary.put Binary.put

instance (Serialize k, Serialize t) => Serialize (DataType k t) where
  get = deserializeWith2 Serialize.get Serialize.get
  put = serializeWith2 Serialize.put Serialize.put
