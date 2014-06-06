{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett and Dan Doel 2012-2013
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Ermine.Syntax.Instance
  ( Instance(..)
  , HasInstance(..)
  ) where

import Control.Applicative
import Control.Lens
import Data.Bifoldable
import Data.Bifunctor
import Data.Bitraversable
import Data.Foldable
import Data.Hashable
import Data.Hashable.Extras
import Data.Monoid
import Data.Typeable
import Data.Void
import Ermine.Syntax.Core
import Ermine.Syntax.Head
import Ermine.Syntax.Id
import Ermine.Syntax.Type
import GHC.Generics

------------------------------------------------------------------------------
-- Instance
------------------------------------------------------------------------------

-- instance Ord a => Ord [a]
-- Instance [ord (pure 0)] (Head ord 0 [star] [] [list (pure 0)]) (LamDict (Scope (Dict [AppDict (InstanceId (Head eq 0 [star] [] [list (pure 0))) (AppDict (Slot 0) (B ()))] ...)))

-- instance Category (:-)
-- Instance [] (Head category 0 [] [constraint] [con ":-" ...]) (Dict [] ...)

data Instance c = Instance
  { _instanceContext :: [Type Void Int]
  , _instanceHead :: Head
  , _instanceBody :: Core c Id
  } deriving (Eq, Typeable, Generic, Show)

makeLensesWith ?? ''Instance $ classyRules & lensClass .~ \_ -> Just ("HasInstance","instance_")

instance HasHead (Instance a) where
  head_ = instanceHead

instance Hashable a => Hashable (Instance a) where
instance Hashable1 Instance where

instance Functor Instance where
  fmap f (Instance c h b) = Instance c h $ first f b

instance Foldable Instance where
  foldMap f (Instance _ _ b) = bifoldMap f (const mempty) b

instance Traversable Instance where
  traverse f (Instance cxt hd b) = Instance cxt hd <$> bitraverse f pure b
