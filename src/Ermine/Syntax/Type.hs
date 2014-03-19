{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE DeriveGeneric #-}
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
  , isRowConstraint
  , FieldName
  -- * Hard Types
  , HardType(..)
  , Typical(..)
  -- * Binding
  , TK
  , abstractKinds
  , instantiateKinds
  , instantiateKindVars
  , bindType
  , bindTK
  , closedType
  , memoverse
  , prepare
  , abstractAll
  -- * Type Variables
  , HasTypeVars(..)
  -- * Type Annotations
  , Annot(..)
  , annot
  -- * Serialization
  , serializeTK
  , deserializeTK
  ) where

import Bound
import Bound.Scope
import Bound.Var
import Control.Lens
import Control.Applicative
import Control.Comonad
import Control.Monad.Identity
import Control.Monad.Trans
import Control.Monad.State hiding (put, get)
import Data.Bifunctor
import Data.Bifoldable
import qualified Data.Binary as Binary
import Data.Binary (Binary)
import Data.Bytes.Serial
import Data.Bytes.Get
import Data.Bytes.Put
import Data.Bitraversable
import Data.Data
import Data.Foldable hiding (all)
import Data.Hashable
import Data.Hashable.Extras
import Data.Monoid
import Data.IntMap hiding (map, filter, null, foldl')
import Data.List (sortBy, elemIndex)
import Data.Map as Map hiding (map, filter, null, foldl')
import Data.Ord (comparing)
import Data.Set as Set hiding (map, filter, null, foldl')
import Data.Set.Lens as Set
import qualified Data.Serialize as Serialize
import Data.Serialize (Serialize)
import Data.String
import Data.Tagged
import Data.Text (Text)
import Data.Void
import Data.Word
import Ermine.Diagnostic
import Ermine.Syntax
import Ermine.Syntax.Digest
import Ermine.Syntax.Hint
import Ermine.Syntax.Global
import Ermine.Syntax.Kind hiding (Var, general)
import qualified Ermine.Syntax.Kind as Kind
import Ermine.Syntax.Scope
import GHC.Generics
import Prelude.Extras
-- mport Text.Trifecta.Diagnostic.Rendering.Prim

-- | A placeholder for a more complicated database fieldname.
type FieldName = Text

-- | A 'Type' that can be compared with mere structural equality.
data HardType
  = Tuple !Int -- (,...,)   :: forall (k :: @). k -> ... -> k -> k -- n >= 2
  | Arrow      -- (->) :: * -> * -> *
  | Con !Global !(Schema Void)
  | ConcreteRho [FieldName]
  deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)

instance Hashable HardType
instance Digestable HardType

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

  concreteRho :: [FieldName] -> t
  concreteRho s = review hardType (ConcreteRho s)
  {-# INLINE concreteRho #-}

instance Typical HardType where
  hardType = id
  {-# INLINE hardType #-}

instance Fun (Type k) where
  -- TODO: make this preserve invariants about 'Forall'.
  _Fun = prism (\(l,r) -> arrow `App` l `App` r) $ \t -> case t of
    HardType Arrow `App` l `App` r -> Right (l, r)
    _                              -> Left t
  {-# INLINE _Fun #-}

instance App (Type k) where
  _App = prism (uncurry App) $ \t -> case t of
    App l r -> Right (l,r)
    _       -> Left t
  {-# INLINE _App #-}

infixl 9 `App`

instance (p ~ Tagged, f ~ Identity) => Tup p f (Type k t) where
  tupled = unto hither
   where hither [x] = x
         hither l = apps (HardType . Tuple $ length l) l

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
  | Forall [Hint] [Hinted (Scope Int Kind k)] (Scope Int (TK k) a) !(Scope Int (TK k) a)
  | Loc !Rendering !(Type k a)
  | Exists [Hint] [Hinted (Scope Int Kind k)] (Scope Int (TK k) a)
  | And [Type k a]
  deriving (Show, Read, Functor, Foldable, Traversable, Typeable, Generic)

instance (Data k, Data a) => Data (Type k a) where
  gfoldl f z (Var a) = z Var `f` a
  gfoldl f z (App l r) = z App `f` l `f` r
  gfoldl f z (HardType t) = z HardType `f` t
  gfoldl f z (Forall hs ks ts b) = z Forall `f` hs `f` ks `f` ts `f` b
  gfoldl f z (Loc l r) = z (Loc l) `f` r
  gfoldl f z (Exists hs ks b) = z Exists `f` hs `f` ks `f` b
  gfoldl f z (And xs) = z And `f` xs
  toConstr _   = error "toConstr"
  gunfold _ _  = error "gunfold"
  dataTypeOf _ = mkNoRepType "Ermie.Syntax.Type"
  dataCast1 f = gcast1 f
  dataCast2 f = gcast2 f

instance Hashable2 Type
instance Hashable k => Hashable1 (Type k)
instance Digestable k => Digestable1 (Type k)

distApp, distHardType, distForall, distExists, distAnd :: Word
distApp      = maxBound `quot` 3
distHardType = maxBound `quot` 5
distForall   = maxBound `quot` 7
distExists   = maxBound `quot` 11
distAnd      = maxBound `quot` 13

instance (Hashable k, Hashable a) => Hashable (Type k a) where
  hashWithSalt n (Var a)             = hashWithSalt n a
  hashWithSalt n (App l r)           = hashWithSalt n l `hashWithSalt` r `hashWithSalt` distApp
  hashWithSalt n (HardType t)        = hashWithSalt n t `hashWithSalt` distHardType
  hashWithSalt n (Forall k tvs cs b) = hashWithSalt n k `hashWithSalt` tvs `hashWithSalt` cs `hashWithSalt` b `hashWithSalt` distForall
  hashWithSalt n (Loc _ ty)          = hashWithSalt n ty
  hashWithSalt n (Exists k tvs cs)   = hashWithSalt n k `hashWithSalt` tvs `hashWithSalt` cs `hashWithSalt` distExists
  hashWithSalt n (And xs)            = hashWithSalt n xs `hashWithSalt` distAnd

instance (Digestable k, Digestable t) => Digestable (Type k t) where
  digest c (Var a)             = digest c (1 :: Word8) `digest` a
  digest c (App f x)           = digest c (2 :: Word8) `digest` f `digest` x
  digest c (HardType h)        = digest c (3 :: Word8) `digest` h
  digest c (Forall k tvs cs b) = digest c (4 :: Word8) `digest` k `digest` tvs `digest` cs `digest` b
  digest c (Loc _ ty)          = digest c ty
  digest c (Exists k tvs cs)   = digest c (5 :: Word8) `digest` k `digest` tvs `digest` cs
  digest c (And xs)            = digest c (6 :: Word8) `digest` xs

-- A helper function for the forall smart constructor. Given a lens to a
-- map of variable ids, abstracts over a variable, choosing a new id in
-- order if necessary. The map should only contain bindings v -> k where
-- k is less than the size of the map.
--
-- Now augmented to generate hints for the chozen variables.
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
forall :: (Ord k, Ord t)
       => (k -> Hint) -> (t -> Hint) -> [k] -> [(t, Kind k)] -> Type k t -> Type k t
       -> Type k t
-- This case is fairly inefficient. Also ugly. The former probably won't matter, but it'd
-- be nice to fix the latter. Not mangling the structure of the terms would, I believe,
-- be quite complicated, though.
forall kh th ks tks cs (Forall n tls ds b) =
  bimap unbound unbound $
    forall kh' th' ks' tks' (mergeConstraints cs' $ fromScope ds) (fromScope b)
 where
 kh' = unvar (n!!) kh
 th' = unvar (void . (tls!!)) th
 cs' = bimap F F cs
 ks' = map F ks ++ zipWith (const . B) [0..] n
 tks' = map (bimap F $ fmap F) tks ++ imap (\i l -> (B i, fromScope $ extract l)) tls
forall kh th ks tks cs body = evalState ?? (Map.empty, Map.empty) $ do
  body' <- typeVars tty body
  tvm  <- use _2
  let tvs = vars tvm
  tks' <- kindVars tkn $ (\v -> tm Map.! v <$ th v) <$> tvs
  body'' <- kindVars tkn body'
  kvm <- use _1
  let kvs = vars kvm
      cs' = abstract (`Map.lookup` tvm)
          . abstractKinds (`Map.lookup` kvm)
          $ exists kh th
                   (filter (`Map.notMember` kvm) ks)
                   (filter (flip Map.notMember tvm . fst) tks)
                   cs
  return $ Forall (kh <$> kvs) (fmap toScope <$> tks') cs' (Scope body'')
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
exists :: (Ord k, Ord t)
       => (k -> Hint) -> (t -> Hint) -> [k] -> [(t, Kind k)] -> Type k t -> Type k t
exists kh th ks tks body = case body of
  Exists n ls b -> process n ls (fromScope b)
  _             -> process [] [] (bimap F F body)
 where
 oks = setOf kindVars body
 ots = setOf typeVars body

 ks'  = filter (`Set.member` oks) ks
 (ts', tks') = unzip $ filter (\(t, _) -> t `Set.member` ots) tks
 htks' = zipWith (<$) tks' (th <$> ts')

 ex n l b
   | not (null n) || not (null l) = Exists n l b
   | otherwise             =
     instantiateKinds panic $ instantiate panic b
  where
  panic _ = error "exists: Panic: unquantified bound variable."

 process kn ls b =
   ex
     (kn ++ map kh ks')
     (ls ++ map (abstract (`elemIndex` ks') <$>) htks')
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
  Exists (m ++ n) (ks ++ ls') . Scope $ mergeConstraints c d'
 where
 bk :: Var Int a -> Var Int a -- mono local binds
 bk = first (+ length m) -- bump kind var
 ls' = fmap (Scope . (fmap bk) . unscope) <$> ls
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

-- | Determines whether the given type is a row constraint.
--
-- As of now, we have no row constraints.
isRowConstraint :: Type k a -> Bool
isRowConstraint _ = False

instance IsString a => IsString (Type k a) where
  fromString = Var . fromString
  {-# INLINE fromString #-}

instance Variable (Type k) where
  _Var = prism Var $ \t -> case t of
    Var a -> Right a
    _     -> Left  t
  {-# INLINE _Var #-}

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
  bitraverse f g (Forall n ks cs b) =
    Forall n <$> traverse (traverse (traverse f)) ks
             <*> bitraverseScope (traverse f) g cs
             <*> bitraverseScope (traverse f) g b
  bitraverse f g (Loc r as)         = Loc r <$> bitraverse f g as
  bitraverse f g (Exists n ks cs)   =
    Exists n <$> traverse (traverse (traverse f)) ks
             <*> bitraverseScope (traverse f) g cs
  bitraverse f g (And cs)           = And <$> traverse (bitraverse f g) cs

instance HasKindVars (Type k a) (Type k' a) k k' where
  kindVars f = bitraverse f pure
  {-# INLINE kindVars #-}

instance Eq k => Eq1 (Type k)
instance Show k => Show1 (Type k)
instance Read k => Read1 (Type k)

instance Eq2 Type
instance Show2 Type
instance Read2 Type

-- | Perform simultaneous substitution on kinds and types in a 'Type'.
bindType :: (k -> Kind k') -> (a -> Type k' b) -> Type k a -> Type k' b
bindType _ g (Var a)             = g a
bindType f g (App l r)           = App (bindType f g l) (bindType f g r)
bindType _ _ (HardType t)        = HardType t
bindType f g (Forall n tks cs b) =
  Forall n (fmap (>>>= f) <$> tks)
           (hoistScope (bindTK f) cs >>>= liftTK . g)
           (hoistScope (bindTK f) b >>>= liftTK . g)
bindType f g (Loc r as)          = Loc r (bindType f g as)
bindType f g (Exists n ks cs)    =
  Exists n (fmap (>>>= f) <$> ks) (hoistScope (bindTK f) cs >>>= liftTK . g)
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

instance (HasTypeVars s t a b, HasTypeVars u v a b) => HasTypeVars (s,u) (t,v) a b where
  typeVars = beside typeVars typeVars
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
abstractKinds f = first k where
  k y = case f y of
    Just z  -> B z
    Nothing -> F y
{-# INLINE abstractKinds #-}

-- | Instantiate the kinds bound by a 'TK' obtaining a 'Type'.
instantiateKinds :: (Int -> Kind k) -> TK k a -> Type k a
instantiateKinds k = bindType go Var where
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
memoverse :: (Bitraversable t, Applicative f, Monad f, Ord k, Ord a)
          => (k -> f (Bool, l)) -> (a -> f (Bool, b))
          -> t k a -> f (t l b)
memoverse knd typ = flip evalStateT (Map.empty, Map.empty) . bitraverse (memoed _1 knd) (memoed _2 typ)
 where
 memoed :: (Ord v, Monad f) => Lens' s (Map v u) -> (v -> f (Bool, u)) -> v -> StateT s f u
 memoed l g v = use l >>= \m -> case m ^. at v of
   Just v' -> return v'
   Nothing -> do (memo, v') <- lift (g v)
                 when memo $ l.at v ?= v'
                 return v'

-- | If a type has no free type or kind variables, the parameters can freely be
-- changed. This function performs that test. See also Bound.close.
closedType :: Type k t -> Maybe (Type k' t')
closedType = bitraverse (const Nothing) (const Nothing)

-- | A function for preparing the sort of type that comes out of parsing for
-- the inference process. Actions for kind and type variables of types 'k' and
-- 't' will only be performed once for each unique value, then memoized and the
-- result re-used. The default action for 'Nothing' kinds will be used at every
-- occurrence, however.
prepare :: forall t f k a l b. (Bitraversable t, Applicative f, Monad f, Ord k, Ord a)
        => f l -> (k -> f l) -> (a -> f b)
        -> t (Maybe k) a -> f (t l b)
prepare unk knd typ =
  memoverse (maybe ((,) False <$> unk) (memoizing knd)) (memoizing typ)

-- | Abstract all the unique variables out of a type with ordered type variables and
-- ordered, possibly unknown kind variables. This yields a scope with possibly unknown
-- kind variables and verifiably no type variables. Also returned are the number of
-- unique kind and type variables
abstractAll :: (Ord k, Ord a)
            => (k -> Hint) -> (a -> Hint)
            -> Type (Maybe k) a -> (Scope Int (TK ()) b, ([Hint], [Hint]))
abstractAll kh th = flip runState ([], []) . fmap Scope . prepare unk kv tv
 where
 unk = pure $ F ()
 kv s = B . length <$> (_1 <<%= (kh s:))
 tv s = B . length <$> (_2 <<%= (th s:))

-- | A type annotation
data Annot k a = Annot [Kind k] !(Scope Int (Type k) a) deriving Show

instance Functor (Annot k) where
  fmap f (Annot ks b) = Annot ks (fmap f b)
  {-# INLINE fmap #-}

instance Foldable (Annot k) where
  foldMap f (Annot _ b) = foldMap f b
  {-# INLINE foldMap #-}

instance Traversable (Annot k) where
  traverse f (Annot ks b) = Annot ks <$> traverse f b
  {-# INLINE traverse #-}

instance Bifunctor Annot where
  bimap = bimapDefault
  {-# INLINE bimap #-}

instance Bifoldable Annot where
  bifoldMap = bifoldMapDefault
  {-# INLINE bifoldMap #-}

instance Bitraversable Annot where
  bitraverse f g (Annot ks b) =
    Annot <$> traverse (traverse f) ks <*> bitraverseScope f g b
  {-# INLINE bitraverse #-}

instance HasKindVars (Annot k a) (Annot k' a) k k' where
  kindVars f = bitraverse f pure
  {-# INLINE kindVars #-}

instance HasTypeVars (Annot k a) (Annot k a') a a' where
  typeVars = traverse
  {-# INLINE typeVars #-}

annot :: Type k a -> Annot k a
annot = Annot [] . lift
{-# INLINE annot #-}

instance Fun (Annot k) where
  _Fun = prism hither yon
    where
    hither (Annot ks (Scope s), Annot ls t) = Annot (ks ++ ls) $
      let Scope t' = mapBound (+ length ks) t
      in Scope (s ~> t')
    yon t@(Annot ks s) = case fromScope s of
      App (App (HardType Arrow) l) r ->
        case (maximumOf (traverse._B) l, minimumOf (traverse._B) r) of
          (Nothing, Nothing) ->
            Right (Annot [] (toScope l), Annot [] (toScope r))
          (Nothing, Just 0)  ->
            Right (Annot [] (toScope l), Annot ks (toScope r))
          (Just m, Nothing)  | length ks == m + 1 ->
            Right (Annot ks (toScope l), Annot [] (toScope r))
          (Just m, Just o)   | m == o - 1 ->
            let (ls, rs) = splitAt o ks in
            Right (Annot ls (toScope l), Annot rs (toScope (r & mapped._B -~ o)))
          _                               -> Left t
      _                                 -> Left t

instance App (Annot k) where
  _App = prism hither yon
    where
    hither (Annot ks (Scope s), Annot ls t) = Annot (ks ++ ls) $
      let Scope t' = mapBound (+ length ks) t
      in Scope (App s t')
    yon t@(Annot ks s) = case fromScope s of
      App l r ->
        case (maximumOf (traverse._B) l, minimumOf (traverse._B) r) of
          (Nothing, Nothing) ->
            Right (Annot [] (toScope l), Annot [] (toScope r))
          (Nothing, Just 0) ->
            Right (Annot [] (toScope l), Annot ks (toScope r))
          (Just m, Nothing) | length ks == m + 1 ->
            Right (Annot ks (toScope l), Annot [] (toScope r))
          (Just m, Just o) | m == o - 1 ->
            let (ls, rs) = splitAt o ks in
            Right (Annot ls (toScope l), Annot rs (toScope (r & mapped._B -~ o)))
          _                               -> Left t
      _                                 -> Left t

instance Variable (Annot k) where
  _Var = prism (annot . return) $ \ t@(Annot _ (Scope b)) -> case b of
    Var (F (Var k)) -> Right k
    _               -> Left  t
  {-# INLINE _Var #-}

instance Typical (Annot k a) where
  hardType = prism (annot . review hardType) $ \ t@(Annot _ (Scope b)) -> case b of
    HardType a           -> Right a
    Var (F (HardType a)) -> Right a
    _                    -> Left t
  {-# INLINE hardType #-}


--------------------------------------------------------------------
-- Serialization
--------------------------------------------------------------------

instance Serial HardType where
  serialize (Tuple n)        = putWord8 0 *> serialize n
  serialize Arrow            = putWord8 1
  serialize (Con g s)        = putWord8 2 *> serialize g *> serializeWith absurd s
  serialize (ConcreteRho fs) = putWord8 3 *> serialize fs

  deserialize = getWord8 >>= \b -> case b of
    0 -> Tuple <$> deserialize
    1 -> pure Arrow
    2 -> Con <$> deserialize
             <*> deserializeWith
                   (fail "deserialize: HardType: getting schema with variables")
    3 -> ConcreteRho <$> deserialize
    _ -> fail $ "deserialize: HardType: unexpected constructor tag: " ++ show b

instance Binary HardType where
  put = serialize
  get = deserialize

instance Serialize HardType where
  put = serialize
  get = deserialize

serializeTK :: MonadPut m => (k -> m ()) -> (t -> m ()) -> TK k t -> m ()
serializeTK = serializeWith2 . serializeWith

deserializeTK :: MonadGet m => m k -> m t -> m (TK k t)
deserializeTK = deserializeWith2 . deserializeWith

instance Serial2 Type where
  serializeWith2 _  pt (Var v)              = putWord8 0 *> pt v
  serializeWith2 _  _  (HardType h)         = putWord8 1 *> serialize h
  serializeWith2 pk pt (Loc _r t)           = putWord8 2 *> serializeWith2 pk pt t
  serializeWith2 pk pt (App f x)            =
    putWord8 3 *> serializeWith2 pk pt f *> serializeWith2 pk pt x
  serializeWith2 pk pt (Forall n ks c body) =
    putWord8 4 *> serialize n *>
    serializeWith (serializeWith (serializeScope serialize pk)) ks *>
    serializeScope3 serialize (serializeTK pk) pt c *>
    serializeScope3 serialize (serializeTK pk) pt body
  serializeWith2 pk pt (Exists n ks body)   =
    putWord8 5 *> serialize n *>
    serializeWith (serializeWith (serializeScope serialize pk)) ks *>
    serializeScope3 serialize (serializeTK pk) pt body
  serializeWith2 pk pt (And ls)             =
    putWord8 6 *> serializeWith (serializeWith2 pk pt) ls

  deserializeWith2 gk gt = getWord8 >>= \b -> case b of
    0 -> Var <$> gt
    1 -> HardType <$> deserialize
    2 -> Loc mempty <$> deserializeWith2 gk gt
    3 -> App <$> deserializeWith2 gk gt <*> deserializeWith2 gk gt
    4 -> Forall <$> deserialize
                <*> deserializeWith (deserializeWith (deserializeScope deserialize gk))
                <*> deserializeScope3 deserialize (deserializeTK gk) gt
                <*> deserializeScope3 deserialize (deserializeTK gk) gt
    5 -> Exists <$> deserialize
                <*> deserializeWith (deserializeWith (deserializeScope deserialize gk))
                <*> deserializeScope3 deserialize (deserializeTK gk) gt
    6 -> And <$> deserializeWith (deserializeWith2 gk gt)
    _ -> fail $ "deserializeWith2: Unexpected constructor tag: " ++ show b

instance Serial k => Serial1 (Type k) where
  serializeWith = serializeWith2 serialize
  deserializeWith = deserializeWith2 deserialize

instance (Serial k, Serial t) => Serial (Type k t) where
  serialize = serializeWith2 serialize serialize
  deserialize = deserializeWith2 deserialize deserialize

instance (Binary k, Binary t) => Binary (Type k t) where
  put = serializeWith2   Binary.put Binary.put
  get = deserializeWith2 Binary.get Binary.get

instance Serial2 Annot where
  serializeWith2 pk pt (Annot ks s) =
    serializeWith (serializeWith pk) ks *>
    serializeScope3 serialize (serializeWith2 pk) pt s

  deserializeWith2 gk gt =
    Annot <$> deserializeWith (deserializeWith gk)
          <*> deserializeScope3 deserialize (deserializeWith2 gk) gt

instance Serial t => Serial1 (Annot t) where
  serializeWith = serializeWith2 serialize
  deserializeWith = deserializeWith2 deserialize

instance (Serial t, Serial v) => Serial (Annot t v) where
  serialize = serializeWith serialize
  deserialize = deserializeWith deserialize

instance (Binary k, Binary t) => Binary (Annot k t) where
  put = serializeWith2   Binary.put Binary.put
  get = deserializeWith2 Binary.get Binary.get

instance HasTypeVars s t a b => HasTypeVars (Hinted s) (Hinted t) a b where
  typeVars = traverse.typeVars

