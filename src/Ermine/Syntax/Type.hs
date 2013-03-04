{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE PatternGuards #-}
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
  , exists
  , mergeConstraints
  , allConstraints
  , (~~>)
  , isTrivialConstraint
  , FieldName
  -- * Hard Types
  , HardType(..)
  , Typical(..)
  -- * Binding
  , TK
  , instantiateKinds
  , instantiateKindVars
  , bindType
  , memoverse
  , prepare
  , abstractAll
  -- * Type Variables
  , HasTypeVars(..)
  -- * Type Annotations
  , Annot(..)
  ) where

import Bound
import Bound.Scope
import Bound.Var
import Control.Lens
import Control.Applicative
import Control.Monad.Trans
import Control.Monad.State
import Data.Bifunctor
import Data.Bifoldable
import Data.Bitraversable
import Data.Foldable hiding (all)
import Data.Ord (comparing)
import Data.IntMap hiding (map, filter, null, foldl')
import Data.List (sortBy, elemIndex)
import Data.Map as Map hiding (map, filter, null, foldl')
import Data.Set as Set hiding (map, filter, null, foldl')
import Data.Set.Lens as Set
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
--
-- For the purposes of type checking and general ease of use, we maintain several
-- invariants privileging otherwise isomorphic types. Those invariants are as follows:
--
--   1. There is never a @forall@ to the right of an arrow; the forall is lifted out:
--        @T -> forall a. U   ==>   forall a. T -> U@
--
--   2. All variables bound by a quantifier are used:
--        @forall a b. a   ==>   forall a. a@
--
--   3. Exists must quantify at least one type or kind variable. @'Exists' 0 []@ should be
--      stripped out of all types:
--        @exists . C   ==>   C@
--
--   4. 'Forall' must either quantify one variable or contain a non-trivial constraint:
--        @forall . T   ==> T@
--
--   5. Quantifiers must not directly contain another quantifier of the same type;
--      they should be coalesced:
--        @forall a. forall b. T   ==>   forall a b. T@
--
--   6. Ands of constraints should not be nested, the lists should be coalesced:
--        @((C, D), E)   ==>   (C, D, E)@
--
--   7. Existentials should be floated out to the top of constraints:
--        @(exists a. C, exists b. D)   ==>   exists a b. (C, D)@
--
--   8. 'Forall' should only quantify variables that occur in the non-constraint
--      portion of the body. Other variables should be pushed into existential
--      quantifiers in the constraints:
--        @forall a b. C b => a   ==>   forall a. (exists b. C b) => a@
--
--   9. The order of the variables in a forall should correspond to that of
--      their use in the rest of the type. For kinds, this includes their
--      appearance in the annotations of the type variables in the same
--      quantifier:
--
-- >      forall {l k} (b : l) (a : k) (g : l -> *) (f : k -> *). f a -> g b
-- >        ==>
-- >      forall {k l} (f : k -> *) (a : k) (g : l -> *) (b : l). f a -> g b
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

unbound :: Var b f -> f
unbound (F v)       = v
unbound (B _)       = error "unbound: B"

-- | A smart constructor for forall. Given a list of kinds, a list of type
-- variables with their kinds, a list of constraints, and a body, abstracts
-- over the kind and type variables in the constraints and the body in
-- the canonical order determined by the body.
forall :: (Ord k, Ord t) => [k] -> [(t, Kind k)] -> Type k t -> Type k t -> Type k t
-- This case is fairly inefficient. Also ugly. The former probably won't matter, but it'd
-- be nice to fix the latter. Not mangling the structure of the terms would, I believe,
-- be quite complicated, though.
forall ks tks cs (Forall n tls ds b) =
  bimap unbound unbound $
    forall ks' tks' (mergeConstraints cs' $ fromScope ds) (fromScope b)
 where
 cs' = bimap F F cs
 ks' = map F ks ++ map B [0 .. n-1]
 tks' = map (bimap F $ fmap F) tks ++ imap (\i l -> (B i, fromScope l)) tls
forall ks tks cs body = evalState ?? (Map.empty, Map.empty) $ do
  body' <- typeVars tty body
  tvm  <- use _2
  let tvs = vars tvm
  tks' <- kindVars tkn $ (tm Map.!) <$> tvs
  body'' <- kindVars tkn body'
  kvm <- use _1
  let kn = Map.size kvm
      cs' = abstract (`Map.lookup` tvm)
          . abstractKinds (`Map.lookup` kvm)
          $ exists (filter (`Map.notMember` kvm) ks)
                   (filter (flip Map.notMember tvm . fst) tks)
                   cs
  return $ Forall kn (toScope <$> tks') cs' (Scope body'')
 where
 km = Set.fromList ks
 tm = Map.fromList tks

 tty t | t `Map.member` tm = B <$> abstractM _2 t
       | otherwise         = return (F . pure $ t)

 tkn k | k `Set.member` km = B <$> abstractM _1 k
       | otherwise         = return (F k)

 vars m = map fst . sortBy (comparing snd) $ Map.toList m

-- | Abstracts over the specified variables using existential quantification.
-- The input type is assumed to meet some invariants (see 'mergeConstraints'),
-- and those invariants are maintained if so.
exists :: (Ord k, Ord t) => [k] -> [(t, Kind k)] -> Type k t -> Type k t
exists ks tks body = case body of
  Exists n ls b -> process n ls (fromScope b)
  _             -> process 0 [] (bimap F F body)
 where
 oks = setOf kindVars body
 ots = setOf typeVars body

 ks'  = filter (`Set.member` oks) ks
 (ts', tks') = unzip $ filter (\(t, _) -> t `Set.member` ots) tks

 ex n l b
   | n > 0 || not (null l) = Exists n l b
   | otherwise             =
     instantiateKinds panic $ instantiate panic b
  where
  panic _ = error "exists: Panic: unquantified bound variable."

 process kn ls b =
   ex
     (kn + length ks')
     (ls ++ map (abstract (`elemIndex` ks')) tks')
     (toScope $ bimap kf tf b)
  where
  tn = length ls
  tf (F v) | Just i <- elemIndex v ts' = B $ i + tn
  tf v                                 = v

  kf (F v) | Just i <- elemIndex v ks' = B $ i + tn
  kf v                                 = v

-- | Takes two constraint types and builds their intersection.
-- The arguments are assumed to follow the invariants that:
--   1) All existentials are outer-most.
--   2) There are no exists of exists
--   3) There are no Ands of Ands
--   4) All quantified variables are actually used in the bodies
-- No attempt is made to simplify the constraints, but the above invariants
-- are maintained.
mergeConstraints :: Type k t -> Type k t -> Type k t
mergeConstraints (Exists m ks (Scope c)) (Exists n ls (Scope d)) =
  Exists (m + n) (ks ++ ls') . Scope $ mergeConstraints c d'
 where
 bk = first (+m) -- bump kind var
 ls' = Scope . fmap bk . unscope <$> ls
 d' = bimap bk (bimap (+ length ks) (first bk)) d
mergeConstraints (Exists m ks (Scope c)) d =
  Exists m ks . Scope $ mergeConstraints c (bimap F (F . pure) d)
mergeConstraints c (Exists m ks (Scope d)) =
  Exists m ks . Scope $ mergeConstraints (bimap F (F . pure) c) d
mergeConstraints (And ls) (And rs) = And $ ls ++ rs
mergeConstraints (And ls) r        = And $ ls ++ [r]
mergeConstraints l        (And rs) = And $ l : rs
mergeConstraints l        r        = And [l, r]

-- | As mergeConstraints, but 0 or more
allConstraints :: [Type k t] -> Type k t
allConstraints = foldl' mergeConstraints (And [])

-- | A smart constructor for function types that moves foralls from the right of an arrow.
-- The right-hand type is assumed to follow the proper invariants for a forall if it is one.
(~~>) :: Type k t -> Type k t -> Type k t
a ~~> Forall kn tks cs body =
  Forall kn tks cs (Scope $ bimap F (F . pure) a ~> unscope body)
a ~~> b = a ~> b

-- | Determines whether the type in question is a trivial constraint, which may be
-- dropped from the type. The simplest example is 'And []', but the function works
-- for non-normalized constraints that are equivalent.
isTrivialConstraint :: Type k a -> Bool
isTrivialConstraint (And cs)        = all isTrivialConstraint cs
isTrivialConstraint (Exists _ _ cs) = isTrivialConstraint $ fromScope cs
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
  bitraverse f g (Forall n ks cs b) = Forall n <$> traverse (traverse f) ks <*> bitraverseScope (traverse f) g cs <*> bitraverseScope (traverse f) g b
  bitraverse f g (Loc r as)         = Loc r <$> bitraverse f g as
  bitraverse f g (Exists n ks cs)   = Exists n <$> traverse (traverse f) ks <*> bitraverseScope (traverse f) g cs
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

-- | 'TK' is a handy alias for dealing with type scopes that bind kind variables. It's a
-- dumber version than the Bound Scope, as we are unsure that the extra nesting pays off
-- relative to the extra effort.
type TK k = Type (Var Int k)

-- | Embed a type which does not reference the freshly bound kinds into 'TK'.
liftTK :: Type k a -> TK k a
liftTK = first F
{-# INLINE liftTK #-}

-- | Substitute kind variables in a 'TK', leaving the bound kinds untouched.
bindTK :: (k -> Kind k') -> TK k a -> TK k' a
bindTK f = bindType (traverse f) Var
{-# INLINE bindTK #-}

-- | Bind some of the kinds referenced by a 'Type'.
-- N.B. This doesn't do any checking to assure that the integers given
-- are in any kind of canonical order, so this should not be used unless
-- that doesn't matter.
abstractKinds :: (k -> Maybe Int) -> Type k a -> TK k a
abstractKinds f t = first k t where
  k y = case f y of
    Just z  -> B z
    Nothing -> F y
{-# INLINE abstractKinds #-}

-- | Instantiate the kinds bound by a 'TK' obtaining a 'Type'.
instantiateKinds :: (Int -> Kind k) -> TK k a -> Type k a
instantiateKinds k e = bindType go Var e where
  go (B b) = k b
  go (F a) = pure a
{-# INLINE instantiateKinds #-}

-- | Instantiate kinds using a list of variables.
instantiateKindVars :: [k] -> TK k a -> Type k a
instantiateKindVars as = first (unvar (as!!) id)
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
abstractAll = flip runState (0, 0) . fmap Scope . prepare unk kv tv
 where
 unk = pure $ F ()
 kv _ = B <$> (_1 <<%= (+1))
 tv _ = B <$> (_2 <<%= (+1))

-- | A type annotation
data Annot k a = Annot {-# UNPACK #-} !Int !(Scope Int (Type k) a)

instance Functor (Annot k) where
  fmap f (Annot n b) = Annot n (fmap f b)
  {-# INLINE fmap #-}

instance Foldable (Annot k) where
  foldMap f (Annot _ b) = foldMap f b
  {-# INLINE foldMap #-}

instance Traversable (Annot k) where
  traverse f (Annot n b) = Annot n <$> traverse f b
  {-# INLINE traverse #-}

instance Bifunctor Annot where
  bimap = bimapDefault
  {-# INLINE bimap #-}

instance Bifoldable Annot where
  bifoldMap = bifoldMapDefault
  {-# INLINE bifoldMap #-}

instance Bitraversable Annot where
  bitraverse f g (Annot n b) = Annot n <$> bitraverseScope f g b
  {-# INLINE bitraverse #-}

instance HasKindVars (Annot k a) (Annot k' a) k k' where
  kindVars f = bitraverse f pure
  {-# INLINE kindVars #-}

instance HasTypeVars (Annot k a) (Annot k a') a a' where
  typeVars = traverse
  {-# INLINE typeVars #-}

annot :: Type k a -> Annot k a
annot = Annot 0 . lift
{-# INLINE annot #-}

instance Fun (Annot k) where
  fun = prism hither yon
    where
    hither (Annot n (Scope s), Annot m t) = Annot (n + m) $
      let Scope t' = mapBound (+n) t
      in Scope (s ~> t')
    yon t@(Annot n s) = case fromScope s of
      App (App (HardType Arrow) l) r -> case (maximumOf (traverse.bound) l, minimumOf (traverse.bound) r) of
        (Nothing, Nothing)              -> Right (Annot 0 (toScope l), Annot 0 (toScope r))
        (Nothing, Just 0)               -> Right (Annot 0 (toScope l), Annot n (toScope r))
        (Just m, Nothing)  | n == m + 1 -> Right (Annot n (toScope l), Annot 0 (toScope r))
        (Just m, Just o)   | m == o - 1 -> Right (Annot (m + 1) (toScope l), Annot (n - o) (toScope (r & mapped.bound -~ o)))
        _                               -> Left t
      _                                 -> Left t

instance App (Annot k) where
  app = prism hither yon
    where
    hither (Annot n (Scope s), Annot m t) = Annot (n + m) $
      let Scope t' = mapBound (+n) t
      in Scope (App s t')
    yon t@(Annot n s) = case fromScope s of
      App l r -> case (maximumOf (traverse.bound) l, minimumOf (traverse.bound) r) of
        (Nothing, Nothing)              -> Right (Annot 0 (toScope l), Annot 0 (toScope r))
        (Nothing, Just 0)               -> Right (Annot 0 (toScope l), Annot n (toScope r))
        (Just m, Nothing)  | n == m + 1 -> Right (Annot n (toScope l), Annot 0 (toScope r))
        (Just m, Just o)   | m == o - 1 -> Right (Annot (m + 1) (toScope l), Annot (n - o) (toScope (r & mapped.bound -~ o)))
        _                               -> Left t
      _                                 -> Left t

instance Variable (Annot k) where
  var = prism (annot . return) $ \ t@(Annot _ (Scope b)) -> case b of
    Var (F (Var k)) -> Right k
    _               -> Left  t
  {-# INLINE var #-}

instance Typical (Annot k a) where
  hardType = prism (annot . review hardType) $ \ t@(Annot _ (Scope b)) -> case b of
    HardType a           -> Right a
    Var (F (HardType a)) -> Right a
    _                    -> Left t
  {-# INLINE hardType #-}
