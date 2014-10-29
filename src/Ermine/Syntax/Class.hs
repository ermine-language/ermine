{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

--------------------------------------------------------------------
---- |
---- Copyright :  (c) Edward Kmett and Dan Doel 2013-2014
---- License   :  BSD2
---- Maintainer:  Edward Kmett <ekmett@gmail.com>
---- Stability :  experimental
---- Portability: non-portable (DeriveDataTypeable)
----
----------------------------------------------------------------------

module Ermine.Syntax.Class
  ( Class(Class)
  , HasClass(..)
  ) where

import Bound
import Bound.Scope
import Control.Applicative
import Control.Lens
import Data.Bifoldable
import Data.Bitraversable
import Data.Map (Map)
import Data.Foldable
import Data.Traversable
import Data.Void
import Ermine.Syntax ((~>))
import Ermine.Syntax.Global
import Ermine.Syntax.Hint
import Ermine.Syntax.Kind as Kind
import Ermine.Syntax.Type as Type
import Ermine.Syntax.Term as Term
import GHC.Generics

data Class k t = Class
               { _kindArgs   :: [Hint]
               , _typeArgs   :: [(Hint, Scope Int Kind k)]
               , _context    :: [Scope Int (TK k) t]
               , _signatures :: Map Global (Type (Var Int k) (Var Int t))
               , _defaults   :: Map Global (Bodies (Annot Void t) Void)
               }
  deriving (Eq, Show, Generic)

makeClassy ''Class

instance Functor (Class k) where
  fmap f (Class kh th cxt sigs defs) =
    Class kh th (fmap f <$> cxt) (fmap (fmap f) <$> sigs) (bimap (fmap f) id <$> defs)

instance Foldable (Class k) where
  foldMap = foldMapDefault

instance Traversable (Class k) where
  traverse f (Class kh th cxt sigs defs) =
    Class kh th <$> (traverse.traverse) f cxt
                <*> (traverse.traverse.traverse) f sigs
                <*> (traverse.flip bitraverse pure.traverse) f defs

instance Bifunctor Class where
  bimap = bimapDefault

instance Bifoldable Class where
  bifoldMap = bifoldMapDefault

instance Bitraversable Class where
  bitraverse f g (Class kh th cxt sigs defs) =
    Class kh <$> (traverse.traverse.traverse) f th
             <*> traverse (bitraverseScope (traverse f) g) cxt
             <*> traverse (bitraverse (traverse f) (traverse g)) sigs
             <*> traverse (bitraverse (traverse g) pure) defs

instance Schematic (Class k t) k where
  schema clazz =
    Schema (clazz^.kindArgs)
           (Scope . Prelude.foldr (~>) constraint $ unscope . snd <$> clazz^.typeArgs)
