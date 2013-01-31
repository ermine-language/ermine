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
-- Module    :  Ermine.Syntax.Type
-- Copyright :  (c) Edward Kmett and Dan Doel 2012
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--------------------------------------------------------------------
module Ermine.Syntax.Type
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
  , instantiateKindVars
  , bindType
  , general
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
import Data.Map as Map hiding (map)
import Data.Set hiding (map)
import Data.String
import Data.Void
import Ermine.Diagnostic
import Ermine.Syntax
import Ermine.Syntax.Global
import Ermine.Syntax.Kind hiding (Var)
import qualified Ermine.Syntax.Kind as Kind
import Ermine.Syntax.Scope
import Prelude.Extras
-- mport Text.Trifecta.Diagnostic.Rendering.Prim

-- | A placeholder for a more complicated database fieldname.
type FieldName = String

-- | A 'Type' that can be compared with mere structural equality.
data HardType
  = Tuple !Int -- (,...,)   :: forall (k :: @). k -> ... -> k -> k -- n >= 2
  | Arrow      -- (->) :: * -> * -> *
  | Con !Global !(Schema Void)
  | ConcreteRho !(Set FieldName)
  deriving (Eq, Ord, Show)

{-
bananas :: Doc a -> Doc a
bananas xs = text "(|" <> xs <> text "|)"

instance Pretty HardType where
  pretty (Tuple i) = parens (replicate (i-1) ',')
  pretty Arrow     = parens ("->")
  pretty (Con (Global _ Idfix p m n) _) = text n
  pretty (ConcreteRho xs) = bananas (fillSep (punctuate (text ",") (map text xs)))

-- | Pretty print a 'Kind', using a fresh kind variable supply and a helper to print free variables
--
-- You should have already removed any free variables from the variable set.
prettySchema :: Applicative f => Schema a -> [String] -> (a -> Bool -> f (Doc b)) -> f (Doc b)
prettySchema (Schema _ b) xs k = prettyKind (fromScope b) False $ \ v p -> case v of
  B i -> pure $! text (xs !! i)
  F a -> k a p
-}



-- | Smart constructors that allows us to pun various 'HardType' constructor names for 'Type'.
class Typical t where
  hardType :: Prism' t HardType

  tuple :: Int -> t
  tuple = review hardType . Tuple
  {-# INLINE tuple #-}

  arrow :: t
  arrow = review hardType Arrow
  {-# INLINE arrow #-}

  con :: Global -> Schema Void -> t
  con g k = review hardType (Con g k)
  {-# INLINE con #-}

  concreteRho :: Set FieldName -> t
  concreteRho s = review hardType (ConcreteRho s)
  {-# INLINE concreteRho #-}

instance Typical HardType where
  hardType = id
  {-# INLINE hardType #-}

instance Fun (Type k) where
  -- TODO: make this preserve invariants about 'Forall'.
  fun = prism (\(l,r) -> arrow `App` l `App` r) $ \t -> case t of
    HardType Arrow `App` l `App` r -> Right (l, r)
    _                              -> Left t
  {-# INLINE fun #-}

instance App (Type k) where
  app = prism (uncurry App) $ \t -> case t of
    App l r -> Right (l,r)
    _       -> Left t
  {-# INLINE app #-}

infixl 9 `App`

-- | Ermine types, parameterized by their free kind variables and free type variables
data Type k a
  = Var a
  | App !(Type k a) !(Type k a)
  | HardType !HardType
  | Forall !Int [Scope Int Kind k] [Scope Int (TK k) a] !(Scope Int (TK k) a)
  | Loc !Rendering !(Type k a)
  | Exists [Kind k] [Scope Int (Type k) a]
  deriving (Show, Functor, Foldable, Traversable)

instance IsString a => IsString (Type k a) where
  fromString = Var . fromString
  {-# INLINE fromString #-}

instance Variable (Type k) where
  var = prism Var $ \t -> case t of
    Var a -> Right a
    _     -> Left  t
  {-# INLINE var #-}

instance Typical (Type k a) where
  hardType = prism HardType $ \ s -> case s of
    HardType a -> Right a
    _          -> Left s
  {-# INLINE hardType #-}

instance (Eq k, Eq a) => Eq (Type k a) where
  Loc _ l          == r                    = l == r
  l                == Loc _ r              = l == r
  Var a            == Var b                = a == b
  App l r          == App l' r'            = l == l' && r == r'
  HardType x       == HardType y           = x == y
  Forall n ks cs b == Forall n' ks' cs' b' = n == n' && ks == ks' && cs == cs' && b == b'
  Exists ks cs     == Exists ks' cs'       = ks == ks' && cs == cs'
  _                == _                    = False

instance Bifunctor Type where
  bimap = bimapDefault
  {-# INLINE bimap #-}

instance Bifoldable Type where
  bifoldMap = bifoldMapDefault
  {-# INLINE bifoldMap #-}

instance Bitraversable Type where
  bitraverse _ g (Var a)            = Var <$> g a
  bitraverse f g (App l r)          = App <$> bitraverse f g l <*> bitraverse f g r
  bitraverse _ _ (HardType t)       = pure $ HardType t
  bitraverse f g (Forall n ks cs b) = Forall n <$> traverse (traverse f) ks <*> traverse (bitraverseScope f g) cs <*> bitraverseScope f g b
  bitraverse f g (Loc r as)         = Loc r <$> bitraverse f g as
  bitraverse f g (Exists ks cs)     = Exists <$> traverse (traverse f) ks <*> traverse (bitraverseScope f g) cs

instance HasKindVars (Type k a) (Type k' a) k k' where
  kindVars f = bitraverse f pure
  {-# INLINE kindVars #-}

instance Eq k => Eq1 (Type k)
instance Show k => Show1 (Type k)

instance Eq2 Type
instance Show2 Type

-- | Perform simultaneous substitution on kinds and types in a 'Type'.
bindType :: (k -> Kind k') -> (a -> Type k' b) -> Type k a -> Type k' b
bindType _ g (Var a)             = g a
bindType f g (App l r)           = App (bindType f g l) (bindType f g r)
bindType _ _ (HardType t)        = HardType t
bindType f g (Forall n tks cs b) = Forall n (map (>>>= f) tks) (map (\c -> hoistScope (bindTK f) c >>>= liftTK . g) cs) (hoistScope (bindTK f) b >>>= liftTK . g)
bindType f g (Loc r as)          = Loc r (bindType f g as)
bindType f g (Exists ks cs)      = Exists (map (>>= f) ks) (map (\c -> hoistScope (bindType f Var) c >>>= g) cs)

instance Applicative (Type k) where
  pure = Var
  {-# INLINE pure #-}
  (<*>) = ap
  {-# INLINE (<*>) #-}

instance Monad (Type k) where
  return = Var
  {-# INLINE return #-}
  m >>= g = bindType Kind.Var g m
  {-# INLINE (>>=) #-}

-- | Provide a 'Traversal' of type variables that can be used to extract them or substitute them for other type variables.
class HasTypeVars s t a b | s -> a, t -> b, s b -> t, t a -> s where
  typeVars :: Traversal s t a b

instance HasTypeVars (Type k a) (Type k b) a b where
  typeVars = traverse
  {-# INLINE typeVars #-}

instance HasTypeVars s t a b => HasTypeVars [s] [t] a b where
  typeVars = traverse.typeVars
  {-# INLINE typeVars #-}

instance HasTypeVars s t a b => HasTypeVars (IntMap s) (IntMap t) a b where
  typeVars = traverse.typeVars
  {-# INLINE typeVars #-}

instance HasTypeVars s t a b => HasTypeVars (Map k s) (Map k t) a b where
  typeVars = traverse.typeVars
  {-# INLINE typeVars #-}

instance HasTypeVars (TK k a) (TK k b) a b where
  typeVars = traverse
  {-# INLINE typeVars #-}

-- | 'TK' is a special version of 'Scope' for types that binds kinds. It is used by 'Forall'.
newtype TK k a = TK { runTK :: Type (Var Int (Kind k)) a } -- TODO: Type (Var Int k) a
  deriving (Eq, Show, Functor, Foldable, Traversable)

-- | Embed a type which does not reference the freshly bound kinds into 'TK'.
liftTK :: Type k a -> TK k a
liftTK = TK . first (F . return)
{-# INLINE liftTK #-}

-- | Substitute kind variables in a 'TK', leaving the bound kinds untouched.
bindTK :: (k -> Kind k') -> TK k a -> TK k' a
bindTK f = TK . bindType (return . fmap (>>= f)) Var . runTK
{-# INLINE bindTK #-}

instance Monad (TK k) where
  return = TK . Var
  {-# INLINE return #-}
  TK t >>= f = TK (t >>= runTK . f)
  {-# INLINE (>>=) #-}

instance Bifunctor TK where
  bimap f g = TK . bimap (fmap (fmap f)) g . runTK
  {-# INLINE bimap #-}

instance Bifoldable TK where
  bifoldMap f g = bifoldMap (foldMap (foldMap f)) g . runTK
  {-# INLINE bifoldMap #-}

instance Bitraversable TK where
  bitraverse f g = fmap TK . bitraverse (traverse (traverse f)) g . runTK
  {-# INLINE bitraverse #-}

instance HasKindVars (TK k a) (TK k' a) k k' where
  kindVars f = bitraverse f pure
  {-# INLINE kindVars #-}

instance Eq k => Eq1 (TK k) where (==#) = (==)
instance Show k => Show1 (TK k) where showsPrec1 = showsPrec

-- | Bind some of the kinds referenced by a 'Type'.
abstractKinds :: (k -> Maybe Int) -> Type k a -> TK k a
abstractKinds f t = TK (first k t) where
  k y = case f y of
    Just z -> B z
    Nothing -> F (return y)
{-# INLINE abstractKinds #-}

-- | Instantiate the kinds bound by a 'TK' obtaining a 'Type'.
instantiateKinds :: (Int -> Kind k) -> TK k a -> Type k a
instantiateKinds k (TK e) = bindType go Var e where
  go (B b) = k b
  go (F a) = a
{-# INLINE instantiateKinds #-}

-- | Instantiate kinds using a list of variables.
instantiateKindVars :: [k] -> TK k a -> Type k a
instantiateKindVars as = instantiateKinds (vs!!) where
  vs = map pure as
{-# INLINE instantiateKindVars #-}

-- | Generalizes a type with comparable variables at the kind and type
-- levels, such as would be the result of parsing. The 'Maybe' at the kind
-- level may be used for unknown annotations, and 'Nothing' maps to units
-- in the output.
general :: (Ord k, Ord a) => Type (Maybe k) a -> Type () b
general ty = Forall kn (replicate tn . Scope . pure $ unknown) [] (Scope $ TK ty')
 where
 unknown = F . pure $ ()
 ((_, _, kn, tn), ty') = bimapAccumL kv tv (Map.empty, Map.empty, 0, 0) ty

 kv st                  Nothing  = (st, unknown)
 kv st@(km, tm, ki, ti) (Just v) = case km^.at v of
   Just u  -> (st, B u)
   Nothing -> let ki' = ki+1 in ki' `seq` ((km & at v ?~ ki, tm, ki', ti), B ki)

 tv st@(km, tm, ki, ti) v = case tm^.at v of
   Just u  -> (st, B u)
   Nothing -> let ti' = ti+1 in ti' `seq` ((km, tm & at v ?~ ti, ki, ti'), B ti)
