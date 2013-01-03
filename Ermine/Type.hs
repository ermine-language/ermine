{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
--------------------------------------------------------------------
-- |
-- Module    :  Ermine.Type
-- Copyright :  (c) Edward Kmett and Dan Doel 2012
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--------------------------------------------------------------------
module Ermine.Type
  (
  -- * Types
    Type(..)
  , FieldName
  -- * Hard Types
  , HardType(..)
  , Typical(..)
  -- * Binding
  , TK(..)
  , abstractKinds
  , instantiateKinds
  , bindType
  -- * Type Variables
  , HasTypeVars(..)
  ) where

import Bound
import Control.Lens
import Control.Applicative
import Control.Monad (ap)
import Data.Bifunctor
import Data.Bifoldable
import Data.Bitraversable
import Data.Foldable
import Data.IntMap hiding (map)
import Data.Map hiding (map)
import Data.Set hiding (map)
import Data.Void
import Ermine.Global
import Ermine.Kind hiding (Var)
import qualified Ermine.Kind as Kind
import Ermine.Mangled
import Ermine.Scope
import Ermine.Rendering
import Ermine.Variable
import Prelude.Extras
-- mport Text.Trifecta.Diagnostic.Rendering.Prim

-- | A placeholder for a more complicated database fieldname.
type FieldName = String

-- | A 'Type' that can be compared with mere structural equality.
data HardType
  = Tuple !Int -- (,...,)   :: forall (k :: @). k -> ... -> k -> k -- n >= 2
  | Arrow      -- (->) :: * -> * -> *
  | Con !Global (Schema Void)
  | ConcreteRho (Set FieldName)
  deriving (Eq, Ord, Show)

-- | Smart constructors that allows us to pun various 'HardType' constructor names for 'Type'.
class Typical t where
  hardType :: Prism' t HardType

  tuple :: Int -> t
  tuple = review hardType . Tuple

  arrow :: t
  arrow = review hardType Arrow

  con :: Global -> Schema Void -> t
  con g k = review hardType (Con g k)

  concreteRho :: Set FieldName -> t
  concreteRho s = review hardType (ConcreteRho s)

instance Typical HardType where
  hardType = id

-- | Ermine types, parameterized by their free kind variables and free type variables
data Type k a
  = Var a
  | App !(Type k a) !(Type k a)
  | HardType HardType
  | Forall !Int [Scope Int Kind k] (Scope Int (TK k) a)
  | Loc !Rendering !(Type k a)
  | Exists [Kind k] [Scope Int (Type k) a]
  deriving (Show, Functor, Foldable, Traversable)

instance Variable (Type k) where
  var = prism Var $ \t -> case t of
    Var a -> Right a
    _     -> Left  t

instance Typical (Type k a) where
  hardType = prism HardType $ \ s -> case s of
    HardType a -> Right a
    _          -> Left s

instance (Eq k, Eq a) => Eq (Type k a) where
  Loc _ l       == r                = l == r
  l             == Loc _ r          = l == r
  Var a         == Var b            = a == b
  App l r       == App l' r'        = l == l' && r == r'
  HardType x    == HardType y       = x == y
  Forall n ks b == Forall n' ks' b' = n == n' && ks == ks' && b == b'
  Exists ks cs  == Exists ks' cs'   = ks == ks' && cs == cs'
  _             == _                = False

instance Bifunctor Type where
  bimap = bimapDefault

instance Bifoldable Type where
  bifoldMap = bifoldMapDefault

instance Bitraversable Type where
  bitraverse _ g (Var a)         = Var <$> g a
  bitraverse f g (App l r)       = App <$> bitraverse f g l <*> bitraverse f g r
  bitraverse _ _ (HardType t)    = pure $ HardType t
  bitraverse f g (Forall n ks b) = Forall n <$> traverse (traverse f) ks <*> bitraverseScope f g b
  bitraverse f g (Loc r as)      = Loc r <$> bitraverse f g as
  bitraverse f g (Exists ks cs)  = Exists <$> traverse (traverse f) ks <*> traverse (bitraverseScope f g) cs

instance HasKindVars (Type k a) (Type k' a) k k' where
  kindVars f = bitraverse f pure

instance Eq k => Eq1 (Type k) where (==#) = (==)
instance Show k => Show1 (Type k) where showsPrec1 = showsPrec

instance Eq2 Type where (==##) = (==)
instance Show2 Type where showsPrec2 = showsPrec

instance Mangled (Type t)

-- | Perform simultaneous substitution on kinds and types in a 'Type'.
bindType :: (k -> Kind k') -> (a -> Type k' b) -> Type k a -> Type k' b
bindType _ g (Var a)          = g a
bindType f g (App l r)        = App (bindType f g l) (bindType f g r)
bindType _ _ (HardType t)     = HardType t
bindType f g (Forall n tks b) = Forall n (map (>>>= f) tks) (hoistScope (bindTK f) b >>>= liftTK . g)
bindType f g (Loc r as)       = Loc r (bindType f g as)
bindType f g (Exists ks cs)   = Exists (map (>>= f) ks) (map (\c -> hoistScope (bindType f Var) c >>>= g) cs)

instance Applicative (Type k) where
  pure = Var
  (<*>) = ap

instance Monad (Type k) where
  return = Var
  m >>= g = bindType Kind.Var g m

-- | Provide a 'Traversal' of type variables that can be used to extract them or substitute them for other type variables.
class HasTypeVars s t a b | s -> a, t -> b, s b -> t, t a -> s where
  typeVars :: Traversal s t a b

instance HasTypeVars (Type k a) (Type k b) a b where
  typeVars = traverse

instance HasTypeVars s t a b => HasTypeVars [s] [t] a b where
  typeVars = traverse.typeVars

instance HasTypeVars s t a b => HasTypeVars (IntMap s) (IntMap t) a b where
  typeVars = traverse.typeVars

instance HasTypeVars s t a b => HasTypeVars (Map k s) (Map k t) a b where
  typeVars = traverse.typeVars

instance HasTypeVars (TK k a) (TK k b) a b where
  typeVars = traverse

-- | 'TK' is a special version of 'Scope' for types that binds kinds. It is used by 'Forall'.
newtype TK k a = TK { runTK :: Type (Var Int (Kind k)) a }
  deriving (Eq, Show, Functor, Foldable, Traversable)

-- | Embed a type which does not reference the freshly bound kinds into 'TK'.
liftTK :: Type k a -> TK k a
liftTK = TK . first (F . return)

-- | Substitute kind variables in a 'TK', leaving the bound kinds untouched.
bindTK :: (k -> Kind k') -> TK k a -> TK k' a
bindTK f = TK . bindType (return . fmap (>>= f)) Var . runTK

instance Monad (TK k) where
  return = TK . Var
  TK t >>= f = TK (t >>= runTK . f)

instance Bifunctor TK where
  bimap f g = TK . bimap (fmap (fmap f)) g . runTK

instance Bifoldable TK where
  bifoldMap f g = bifoldMap (foldMap (foldMap f)) g . runTK

instance Bitraversable TK where
  bitraverse f g = fmap TK . bitraverse (traverse (traverse f)) g . runTK

instance HasKindVars (TK k a) (TK k' a) k k' where
  kindVars f = bitraverse f pure

instance Eq k => Eq1 (TK k) where (==#) = (==)
instance Show k => Show1 (TK k) where showsPrec1 = showsPrec

-- instance BoundBy (TK k) (Type k) where
--   bindBy f (TK b) =

-- instance MangledBy (TK k) (Type k) where
--   mangledBy f (TK b) =

-- | Bind some of the kinds referenced by a 'Type'.
abstractKinds :: (k -> Maybe Int) -> Type k a -> TK k a
abstractKinds f t = TK (first k t) where
  k y = case f y of
    Just z -> B z
    Nothing -> F (return y)

-- | Instantiate the kinds bound by a 'TK' obtaining a 'Type'.
instantiateKinds :: (Int -> Kind k) -> TK k a -> Type k a
instantiateKinds k (TK e) = bindType go Var e where
  go (B b) = k b
  go (F a) = a
