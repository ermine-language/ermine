{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett and Dan Doel 2012-2014
-- License   :  BSD3
-- Maintainer:  Dan Doel <dan.doel@gmail.com>
-- Stability :  experimental
-- Portability: non-portable (DeriveDataTypeable)
--
-- This module provides the AST for data type declarations
--------------------------------------------------------------------

module Ermine.Syntax.Constructor
  ( Constructor(Constructor)
  , ekinds
  , etypes
  , fields
  ) where

import Bound
import Bound.Scope
import Control.Applicative
import Control.Lens
import Data.Bifoldable
import Data.Bifunctor
import Data.Bitraversable
import Data.Binary as Binary
import Data.Bytes.Serial
import Data.Foldable (Foldable)
import Data.Typeable
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

instance BoundBy (Constructor k) (Type k) where
  boundBy f (Constructor tg ks ts fs) = Constructor tg ks ts $ map (>>>= first F . f) fs

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
