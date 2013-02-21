{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
  , forall
  , isTrivialConstraint
  , FieldName
  -- * Hard Types
  , HardType(..)
  , Typical(..)
  -- * Binding
  , TK(..)
  , instantiateKinds
  , instantiateKindVars
  , bindType
  , memoverse
  , prepare
  , abstractAll
  -- * Type Variables
  , HasTypeVars(..)
  ) where

import Bound
import Control.Lens
import Control.Applicative
import Control.Monad.Trans
import Control.Monad.State
import Data.Bifunctor
import Data.Bifoldable
import Data.Bitraversable
import Data.Foldable hiding (all)
import Data.Ord (comparing)
import Data.IntMap hiding (map)
import Data.List (sortBy)
import Data.Map as Map hiding (map)
import Data.Maybe
import Data.Set as Set hiding (map)
import Data.String
import Data.Void
import Ermine.Diagnostic
import Ermine.Syntax
import Ermine.Syntax.Global
import Ermine.Syntax.Kind hiding (Var, general)
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
  | Forall !Int [Scope Int Kind k] (Scope Int (TK k) a) !(Scope Int (TK k) a)
  | Loc !Rendering !(Type k a)
  | Exists !Int [Scope Int Kind k] (Scope Int (TK k) a)
  | And [Type k a]
  deriving (Show, Functor, Foldable, Traversable)

-- A helper function for the forall smart constructor. Given a lens to a
-- map of variable ids, abstracts over a variable, choosing a new id in
-- order if necessary. The map should only contain bindings v -> k where
-- k is less than the size of the map.
abstractM :: (MonadState s m, Ord v) => Lens' s (Map v Int) -> v -> m Int
abstractM l v = use l >>= \m -> case m ^. at v of
  Just i  -> return i
  Nothing -> let i = Map.size m
                 m' = m & at v ?~ i
              in l .= m' >> return i

-- | A smart constructor for forall. Given a list of kinds, a list of type
-- variables with their kinds, a list of constraints, and a body, abstracts
-- over the kind and type variables in the constraints and the body in
-- the canonical order determined by the body.
forall :: (Ord k, Ord t) => (k -> Bool) -> (t -> Maybe (Kind k)) -> Type k t -> Type k t -> Type k t
forall kp tkp cs (Forall _ ks ds b) = bimap fromRight fromRight $ forall kp' tkp' ds' (itk b)
 where
 fromRight (Right x) = x
 fromRight _         = error "fromRight: Left"
 itk = instantiateKinds (pure . Left) . over kindVars Right . instantiate (TK . pure . Left) . fmap Right
 ik  = instantiate (pure . Left) . fmap Right
 cs' = bimap Right Right cs
 ds' = itk ds
 kp' = either (const True) kp
 ks' = ik <$> ks
 tkp' (Left  i) = Just $ ks' !! i
 tkp' (Right v) = fmap (fmap Right) $ tkp v
forall kp tkp cs body = evalState ?? (Map.empty, Map.empty) $ do
  body' <- typeVars tty body
  tm  <- use _2
  let tvs = vars tm
  tks <- kindVars tkn $ (fromJust . tkp) <$> tvs
  body'' <- kindVars tkn body'
  km <- use _1
  let kn = Map.size km
  return $ Forall kn
                  (Scope <$> tks)
                  (abstract (`Map.lookup` tm) . abstractKinds (`Map.lookup` km) $ cs)
                  (Scope . TK $ body'')
 where
 tty t | isJust $ tkp t = B <$> abstractM _2 t
       | otherwise      = return (F . TK . pure $ t)

 tkn k | kp k      = B <$> abstractM _1 k
       | otherwise = return (F . pure $ k)

 vars m = map fst . sortBy (comparing snd) $ Map.toList m

-- | Determines whether the type in question is a trivial constraint, which may be
-- dropped from the type. The simplest example is 'And []', but the function works
-- for non-normalized constraints that are equivalent.
isTrivialConstraint :: Type k a -> Bool
isTrivialConstraint (And cs)        = all isTrivialConstraint cs
isTrivialConstraint (Exists _ _ cs) = isTrivialConstraint (runTK . fromScope $ cs)
isTrivialConstraint _               = False

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
  Exists n ks cs   == Exists n' ks' cs'    = n == n' && ks == ks' && cs == cs'
  And cs           == And cs'              = cs == cs'
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
  bitraverse f g (Forall n ks cs b) = Forall n <$> traverse (traverse f) ks <*> bitraverseScope f g cs <*> bitraverseScope f g b
  bitraverse f g (Loc r as)         = Loc r <$> bitraverse f g as
  bitraverse f g (Exists n ks cs)   = Exists n <$> traverse (traverse f) ks <*> bitraverseScope f g cs
  bitraverse f g (And cs)           = And <$> traverse (bitraverse f g) cs

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
bindType f g (Forall n tks cs b) = Forall n (map (>>>= f) tks) (hoistScope (bindTK f) cs >>>= liftTK . g) (hoistScope (bindTK f) b >>>= liftTK . g)
bindType f g (Loc r as)          = Loc r (bindType f g as)
bindType f g (Exists n ks cs)    = Exists n (map (>>>= f) ks) (hoistScope (bindTK f) cs >>>= liftTK . g)
bindType f g (And cs)            = And $ bindType f g <$> cs

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
-- N.B. This doesn't do any checking to assure that the integers given
-- are in any kind of canonical order, so this should not be used unless
-- that doesn't matter.
abstractKinds :: (k -> Maybe Int) -> Type k a -> TK k a
abstractKinds f t = TK (first k t) where
  k y = case f y of
    Just z  -> B z
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

-- A local helper function for use with memoverse
memoizing :: (Functor f) => (a -> f b) -> a -> f (Bool, b)
memoizing f = fmap ((,) True) . f

-- | A version of bitraverse that allows the functions used to specify whether
-- their results at each value should be remembered, so that the action is not
-- performed additional times at that value.
memoverse :: (Applicative f, Monad f, Ord k, Ord a)
          => (k -> f (Bool, l)) -> (a -> f (Bool, b))
          -> Type k a -> f (Type l b)
memoverse knd typ = flip evalStateT (Map.empty, Map.empty) . bitraverse (memoed _1 knd) (memoed _2 typ)
 where
 memoed :: (Ord v, Monad f) => Lens' s (Map v u) -> (v -> f (Bool, u)) -> v -> StateT s f u
 memoed l g v = use l >>= \m -> case m ^. at v of
   Just v' -> return v'
   Nothing -> do (store, v') <- lift (g v)
                 when store $ l.at v ?= v'
                 return v'

-- | A function for preparing the sort of type that comes out of parsing for
-- the inference process. Actions for kind and type variables of types 'k' and
-- 't' will only be performed once for each unique value, then memoized and the
-- result re-used. The default action for 'Nothing' kinds will be used at every
-- occurrence, however.
prepare :: forall f k a l b. (Applicative f, Monad f, Ord k, Ord a)
        => f l -> (k -> f l) -> (a -> f b)
        -> Type (Maybe k) a -> f (Type l b)
prepare unk knd typ =
  memoverse (maybe ((,) False <$> unk) (memoizing knd)) (memoizing typ)

-- | Abstract all the unique variables out of a type with ordered type variables and
-- ordered, possibly unknown kind variables. This yields a scope with possibly unknown
-- kind variables and verifiably no type variables. Also returned are the number of
-- unique kind and type variables
abstractAll :: (Ord k, Ord a) => Type (Maybe k) a -> (Scope Int (TK ()) b, (Int, Int))
abstractAll = flip runState (0, 0) . fmap (Scope . TK) . prepare unk kv tv
 where
 unk = pure . F . pure $ ()
 kv _ = B <$> (_1 <<%= (+1))
 tv _ = B <$> (_2 <<%= (+1))
