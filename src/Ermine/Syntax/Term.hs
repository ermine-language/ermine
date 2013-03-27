{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
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
-- Module    :  Ermine.Syntax.Term
-- Copyright :  (c) Edward Kmett and Dan Doel 2012
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable (DeriveDataTypeable)
--
-- This module provides the AST for Terms
--------------------------------------------------------------------
module Ermine.Syntax.Term
  (
  -- * Terms
    Term(..)
  , bindTerm
  -- * Hard Terms
  , HardTerm(..)
  , Terminal(..)
  -- * Bindings
  , DeclBound(..)
  , Binding(..)
  , BindingType(..)
  , Body(..)
  , Guarded(..)
  ) where

import Bound
import Bound.Var
import Control.Lens
import Control.Lens.Internal.Review
import Control.Applicative
import Control.Monad.Identity
import Data.Bifoldable
import Data.Binary as Binary
import Data.Bitraversable
import Data.Foldable
import Data.IntMap hiding (map)
import Data.Map hiding (map)
import Data.Monoid
import Data.String
import Ermine.Diagnostic
import Ermine.Syntax
import Ermine.Syntax.Global
import Ermine.Syntax.Kind hiding (Var)
import Ermine.Syntax.Pattern
import Ermine.Syntax.Literal
import Ermine.Syntax.Scope
import Ermine.Syntax.Type hiding (App, Loc, Var, Tuple)
import Prelude.Extras
-- import Text.Trifecta.Diagnostic.Rendering.Prim

-- | Simple terms that can be compared with structural equality.
data HardTerm
  = Lit Literal
  | DataCon !Global
  | Tuple !Int      -- (,,)
  | Hole            -- ^ A placeholder that can take any type. Easy to 'Remember'.
  deriving (Eq, Show)

instance Binary HardTerm where
  put (Lit l)     = putWord8 0 *> put l
  put (DataCon g) = putWord8 1 *> put g
  put (Tuple i)   = putWord8 2 *> put i
  put Hole        = putWord8 3

  get = getWord8 >>= \b -> case b of
    0 -> Lit     <$> get
    1 -> DataCon <$> get
    2 -> Tuple   <$> get
    3 -> return  Hole
    _ -> fail    $ "get HardTerm: Unexpected constructor code: " ++ show b

-- | This class provides a prism to match against or inject a 'HardTerm'.
class Terminal t where
  hardTerm :: Prism' t HardTerm

  litTerm :: Literal -> t
  litTerm = review hardTerm . Lit

  hole :: t
  hole = review hardTerm Hole

instance Terminal HardTerm where
  hardTerm = id

-- | Indicate if a definition is explicitly bound with a type annotation or implicitly bound without.
data BindingType t
  = Explicit t
  | Implicit
  deriving (Eq, Show, Functor, Foldable, Traversable)

instance Binary t => Binary (BindingType t) where
  put = putBindingType put
  get = getBindingType get

putBindingType :: (t -> Put) -> BindingType t -> Put
putBindingType pt (Explicit t) = putWord8 0 *> pt t
putBindingType _   Implicit    = putWord8 1

getBindingType :: Get t -> Get (BindingType t)
getBindingType gt = getWord8 >>= \b -> case b of
  0 -> Explicit <$> gt
  1 -> return Implicit
  _ -> fail $ "getBindingType: Unexpected constructor code: " ++ show b

-- | Bound variables in a declaration are rather complicated. One can refer
-- to any of the following:
--   1. Definitions in the same declaration sequence
--   2. Variables bound in a pattern
--   3. Definitions in a where clause
-- the 'DeclBound' type captures these three cases in the respective constructors.
data DeclBound = D Int | P Int | W Int deriving (Eq,Ord,Show,Read)

instance Binary DeclBound where
  put (D i) = putWord8 0 *> put i
  put (P i) = putWord8 1 *> put i
  put (W i) = putWord8 2 *> put i
  get = getWord8 >>= \b -> case b of
          0 -> D <$> get
          1 -> P <$> get
          2 -> W <$> get
          _ -> fail $ "get DeclBound: Unexpected constructor code: " ++ show b

-- | A body is the right hand side of a definition. This isn't a term because it has to perform simultaneous
-- matches on multiple patterns with backtracking.
-- Each Body contains a list of where clause bindings to which the body and
-- guards can refer.
data Body t a = Body [Pattern t] (Guarded (Scope DeclBound (Term t) a)) [Binding t (Var Int a)]
  deriving (Eq, Show, Functor, Foldable, Traversable)

instance (Binary t, Binary a) => Binary (Body t a) where
  put = putBody put put
  get = getBody get get

putBody :: (t -> Put) -> (a -> Put) -> Body t a -> Put
putBody pt pa (Body pats g bs) =
  putMany (putPat pt) pats *>
  putGuarded (putScope put (putTerm pt) pa) g *>
  putMany (putBinding pt (putVar put pa)) bs

getBody :: Get t -> Get a -> Get (Body t a)
getBody gt ga = Body <$>
  getMany (getPat gt) <*>
  getGuarded (getScope get (getTerm gt) ga) <*>
  getMany (getBinding gt (getVar get ga))

-- | A datatype for representing potentially guarded cases of a function
-- body.
data Guarded tm = Unguarded tm
                | Guarded [(tm, tm)]
  deriving (Eq, Ord, Show, Read, Functor, Foldable, Traversable)

instance Binary tm => Binary (Guarded tm) where
  put = putGuarded put
  get = getGuarded get

putGuarded :: (tm -> Put) -> Guarded tm -> Put
putGuarded ptm (Unguarded tm) = putWord8 0 *> ptm tm
putGuarded ptm (Guarded tms)  = putWord8 1 *> putMany putPair tms where
  putPair (tm1, tm2) = ptm tm1 *> ptm tm2

getGuarded :: Get t -> Get (Guarded t)
getGuarded gtm = getWord8 >>= \b -> case b of
  0 -> Unguarded <$> gtm
  1 -> Guarded <$> getMany ((,) <$> gtm <*> gtm)
  _ -> fail $ "getGuarded: Unexpected constructor code: " ++ show b

instance Bifunctor Body where
  bimap = bimapDefault

instance Bifoldable Body where
  bifoldMap = bifoldMapDefault

instance Bitraversable Body where
  bitraverse f g (Body ps ss wh) =
    Body <$> traverse (traverse f) ps
         <*> traverse (bitraverseScope f g) ss
         <*> traverse (bitraverse f (traverse g)) wh

-- | A Binding provides its source location as a rendering, knowledge of if it is explicit or implicitly bound
-- and a list of right hand side bindings.
data Binding t a = Binding !Rendering !(BindingType t) [Body t a]
  deriving (Show, Functor, Foldable, Traversable)

instance (Binary t, Binary a) => Binary (Binding t a) where
  put = putBinding put put
  get = getBinding get get

putBinding :: (t -> Put) -> (a -> Put) -> Binding t a -> Put
putBinding pt pa (Binding _ bt body) = putBindingType pt bt *> putMany (putBody pt pa) body

getBinding :: Get t -> Get a -> Get (Binding t a)
getBinding gt ga = Binding <$> return mempty <*> getBindingType gt <*> getMany (getBody gt ga)

instance (Eq t, Eq a) => Eq (Binding t a) where
  Binding _ t bs == Binding _ t' bs' = t == t' && bs == bs'

instance Bifunctor Binding where
  bimap = bimapDefault

instance Bifoldable Binding where
  bifoldMap = bifoldMapDefault

instance Bitraversable Binding where
  bitraverse f g (Binding l bt bs) = Binding l <$> traverse f bt <*> traverse (bitraverse f g) bs

-- | Terms in the Ermine language.
data Term t a
  = Var a
  | App !(Term t a) !(Term t a)
  | HardTerm !HardTerm
  | Sig !(Term t a) t
  | Lam [Pattern t] !(Scope Int (Term t) a)
  | Case !(Term t a) [Alt t (Term t) a]
  | Let [Binding t a] !(Scope Int (Term t) a)
  | Loc !Rendering !(Term t a) -- ^ informational link to where the term came from
  | Remember !Int !(Term t a) -- ^ Used to provide hole support.
  deriving (Show, Functor, Foldable, Traversable)

instance IsString a => IsString (Term t a) where
  fromString = Var . fromString

instance Variable (Term t) where
  var = prism Var $ \t -> case t of
    Var a -> Right a
    _     -> Left  t

instance App (Term t a) where
  app = prism (uncurry App) $ \t -> case t of
    App l r -> Right (l,r)
    _       -> Left t

instance (p ~ Reviewed, f ~ Identity) => Tup p f (Term t a) where
  tupled = unto hither
   where hither [x] = x
         hither l = apps (HardTerm . Tuple $ length l) l

instance Terminal (Term t a) where
  hardTerm = prism HardTerm $ \t -> case t of
    HardTerm a -> Right a
    _          -> Left t

instance (Eq t, Eq a) => Eq (Term t a) where
  Loc _ l      == r            = l == r
  l            == Loc _ r      = l == r

  Remember _ l == r            = l == r -- ?
  l            == Remember _ r = l == r -- ?

  Var a        == Var b        = a == b
  Sig e t      == Sig e' t'    = e == e' && t == t'
  Lam p b      == Lam p' b'    = p == p' && b == b'
  HardTerm t   == HardTerm t'  = t == t'
  Case b as    == Case b' as'  = b == b' && as == as'
  App a b      == App c d      = a == c  && b == d
  _            == _            = False

instance Bifunctor Term where
  bimap = bimapDefault

instance Bifoldable Term where
  bifoldMap = bifoldMapDefault

instance Bitraversable Term where
  bitraverse f g = tm where
    tm (Var a)        = Var <$> g a
    tm (Sig e t)      = Sig <$> tm e <*> f t
    tm (Lam ps b)     = Lam <$> traverse (traverse f) ps <*> bitraverseScope f g b
    tm (HardTerm t)   = pure (HardTerm t)
    tm (App l r)      = App <$> tm l <*> tm r
    tm (Loc r b)      = Loc r <$> tm b
    tm (Remember i b) = Remember i <$> tm b
    tm (Case b as)    = Case <$> tm b <*> traverse (bitraverseAlt f g) as
    tm (Let bs ss)    = Let <$> traverse (bitraverse f g) bs <*> bitraverseScope f g ss
  {-# INLINE bitraverse #-}

instance Eq t => Eq1 (Term t)
instance Show t => Show1 (Term t)

instance Eq2 Term
instance Show2 Term

-- | Perform simultaneous substitution on terms and type annotations.
bindTerm :: (t -> t') -> (a -> Term t' b) -> Term t a -> Term t' b
bindTerm _ g (Var a)   = g a
bindTerm f g (App l r) = App (bindTerm f g l) (bindTerm f g r)
bindTerm f g (Sig e t) = Sig (bindTerm f g e) (f t)
bindTerm _ _ (HardTerm t) = HardTerm t
bindTerm f g (Lam ps (Scope b)) = Lam (fmap f <$> ps) (Scope (bimap f (fmap (bindTerm f g)) b))
bindTerm f g (Loc r b) = Loc r (bindTerm f g b)
bindTerm f g (Remember i b) = Remember i (bindTerm f g b)
bindTerm f g (Case b as) = Case (bindTerm f g b) (bindAlt f g <$> as)
bindTerm f g (Let bs (Scope b)) = Let (bindBinding f g <$> bs) (Scope (bimap f (fmap (bindTerm f g)) b))

bindBody :: (t -> t') -> (a -> Term t' b) -> Body t a -> Body t' b
bindBody f g (Body ps gs wh) =
  let s (Scope b) = Scope $ bimap f (fmap $ bindTerm f g) b in
    Body (fmap f <$> ps)
         (s <$> gs)
         (fmap (bindBinding f (unvar (pure . B) (fmap F . g))) wh)

bindBinding :: (t -> t') -> (a -> Term t' b) -> Binding t a -> Binding t' b
bindBinding f g (Binding r bt bs) = Binding r (fmap f bt) (bindBody f g <$> bs)

bindAlt :: (t -> t') -> (a -> Term t' b) -> Alt t (Term t) a -> Alt t' (Term t') b
bindAlt f g (Alt p (Scope b)) = Alt (fmap f p) (Scope (bindTerm f (Var . fmap (bindTerm f g)) b))

instance Applicative (Term t) where
  pure = Var
  (<*>) = ap

instance Monad (Term t) where
  return = Var
  m >>= g = bindTerm id g m

------------------------------------------------------------------------------
-- Variables
------------------------------------------------------------------------------

instance HasKindVars t t' k k' => HasKindVars (Term t a) (Term t' a) k k' where
  kindVars f = bitraverse (kindVars f) pure

instance HasTypeVars t t' tv tv' => HasTypeVars (Term t a) (Term t' a) tv tv' where
  typeVars f = bitraverse (typeVars f) pure

-- | Provides a traversal of term variables for variable->variable substitution or extracting free variables.
class HasTermVars s t a b | s -> a, t -> b, s b -> t, t a -> s where
  termVars :: Traversal s t a b

instance HasTermVars (Term t a) (Term t b) a b where
  termVars = traverse

instance HasTermVars s t a b => HasTermVars [s] [t] a b where
  termVars = traverse.termVars

instance HasTermVars s t a b => HasTermVars (IntMap s) (IntMap t) a b where
  termVars = traverse.termVars

instance HasTermVars s t a b => HasTermVars (Map k s) (Map k t) a b where
  termVars = traverse.termVars


putMany :: (k -> Put) -> [k] -> Put
putMany p ls = put (length ls) *> traverse_ p ls

getMany :: Get k -> Get [k]
getMany g = get >>= \n -> replicateM n g

-- | Binary serialization of a 'Term', given serializers for its two
-- parameters.
putTerm :: (t -> Put) -> (a -> Put) -> Term t a -> Put
putTerm _  pa (Var a)        = putWord8 0 *> pa a
putTerm pt pa (App t1 t2)    = putWord8 1 *> putTerm pt pa t1 *> putTerm pt pa t2
putTerm _ _   (HardTerm h)   = putWord8 2 *> put h
putTerm pt pa (Sig t1 t)     = putWord8 3 *> putTerm pt pa t1 *> pt t
putTerm pt pa (Lam ps s)     = putWord8 4 *> putMany (putPat pt) ps *> putScope put (putTerm pt) pa s
putTerm pt pa (Case t alts)  = putWord8 5 *> putTerm pt pa t *> putMany (putAlt pt (putTerm pt) pa) alts
putTerm pt pa (Let bs s)     = putWord8 6 *> putMany (putBinding pt pa) bs *> putScope put (putTerm pt) pa s
putTerm pt pa (Loc _ t)      = putWord8 7 *> putTerm pt pa t
putTerm pt pa (Remember i t) = putWord8 8 *> put i *> putTerm pt pa t

getTerm :: Get t -> Get a -> Get (Term t a)
getTerm gt ga = getWord8 >>= \b -> case b of
  0 -> Var <$> ga
  1 -> App <$> getTerm gt ga <*> getTerm gt ga
  2 -> HardTerm <$> get
  3 -> Sig  <$> getTerm gt ga <*> gt
  4 -> Lam  <$> getMany (getPat gt) <*> getScope get (getTerm gt) ga
  5 -> Case <$> getTerm gt ga <*> getMany (getAlt gt (getTerm gt) ga)
  6 -> Let  <$> getMany (getBinding gt ga) <*> getScope get (getTerm gt) ga
  7 -> Loc  <$> return mempty <*> getTerm gt ga
  8 -> Remember <$> get <*> getTerm gt ga
  _ -> fail $ "getGuarded: Unexpected constructor code: " ++ show b

instance (Binary k, Binary t) => Binary (Term k t) where
  put = putTerm put put
  get = getTerm get get
