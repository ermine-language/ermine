{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
--------------------------------------------------------------------
-- |
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
  , BodyBound(..)
  , WhereBound(..)
  , AsDecl(..)
  , Binding(..)
  , HasBinding(..)
  , BindingType(..)
  , _Implicit
  , _Explicit
  , Body(..)
  , bodyDecls
  ) where

import Bound
import Bound.Scope
import Bound.Var
import Control.Lens
import Control.Applicative
import Control.Monad.Identity
import Data.Bifoldable
import qualified Data.Binary as Binary
import Data.Binary (Binary)
import Data.Bitraversable
import Data.Bytes.Get
import Data.Bytes.Put
import Data.Bytes.Serial
import Data.Foldable
import Data.IntMap hiding (map)
import Data.Map hiding (map)
import Data.Monoid
import Data.String
import qualified Data.Serialize as Serialize
import Data.Serialize (Serialize)
import Data.Tagged
import Data.Void
import Data.Word
import Ermine.Diagnostic
import Ermine.Syntax
import Ermine.Syntax.Global
import Ermine.Syntax.Kind hiding (Var)
import Ermine.Syntax.Pattern
import Ermine.Syntax.Literal
import Ermine.Syntax.Scope
import Ermine.Syntax.Type hiding (App, Loc, Var, Tuple)
import GHC.Generics
import Prelude.Extras
-- import Text.Trifecta.Diagnostic.Rendering.Prim

-- | Simple terms that can be compared with structural equality.
data HardTerm
  = Lit Literal
  | DataCon !Global (Type Void Void)
  | Tuple !Word8    -- (,,)
  | Hole            -- ^ A placeholder that can take any type. Easy to 'Remember'.
  deriving (Eq, Show, Generic)

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
  deriving (Eq, Show, Functor, Foldable, Traversable, Generic)

-- | Bound variables in a declaration are rather complicated. One can refer
-- to any of the following:
--   1. Definitions in the same declaration sequence
--   2. Variables bound in a pattern
--   3. Definitions in a where clause
-- the 'DeclBound' type captures these three cases in the respective constructors.
data BodyBound = BodyDecl Word32
               | BodyPat PatPath
               | BodyWhere Word32
  deriving (Eq,Ord,Show,Read,Generic)

data WhereBound = WhereDecl Word32
                | WherePat PatPath
  deriving (Eq,Ord,Show,Read,Generic)

class AsDecl t where
  _Decl :: Prism' t Word32

instance AsDecl BodyBound where
  _Decl = prism BodyDecl $ \case
    BodyDecl d -> Right d
    x          -> Left x

instance AsDecl WhereBound where
  _Decl = prism WhereDecl $ \case
    WhereDecl d -> Right d
    x           -> Left x

-- | A body is the right hand side of a definition. This isn't a term because it has to perform simultaneous
-- matches on multiple patterns with backtracking.
-- Each Body contains a list of where clause bindings to which the body and
-- guards can refer.
data Body t a = Body [Pattern t]
                     (Guarded (Scope BodyBound (Term t) a))
                     [Binding t (Var WhereBound a)]
  deriving (Eq, Show, Functor, Foldable, Traversable)

bodyDecls :: Traversal' (Body t a) Word32
bodyDecls f (Body ps g bs) = Body ps <$> (traverse.traverseBound._Decl) f g <*> (traverse.traverse._B._Decl) f bs

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
data Binding t a = Binding { _bindingLoc :: !Rendering, _bindingType :: !(BindingType t), _bindingBodies :: [Body t a] }
  deriving (Show, Functor, Foldable, Traversable)

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
  | Lam [Pattern t] !(Scope PatPath (Term t) a)
  | Case !(Term t a) [Alt t (Term t) a]
  | Let [Binding t a] !(Scope Int (Term t) a)
  | Loc !Rendering !(Term t a) -- ^ informational link to the location the term came from
  | Remember !Int !(Term t a) -- ^ Used to provide hole support.
  deriving (Show, Functor, Foldable, Traversable)

instance IsString a => IsString (Term t a) where
  fromString = Var . fromString

instance Variable (Term t) where
  _Var = prism Var $ \t -> case t of
    Var a -> Right a
    _     -> Left  t
  {-# INLINE _Var #-}

instance App (Term t) where
  _App = prism (uncurry App) $ \t -> case t of
    App l r -> Right (l,r)
    _       -> Left t
  {-# INLINE _App #-}

instance (p ~ Tagged, f ~ Identity) => Tup p f (Term t a) where
  _Tup = unto hither
   where hither [x] = x
         hither l = apps (HardTerm . Tuple . fromIntegral $ length l) l

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
  Let bs e     == Let bs' e'   = bs == bs' && e == e' -- this is rather inflexible
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
bindAlt f g (Alt p gs) =
  let s (Scope b) = Scope $ bimap f (fmap $ bindTerm f g) b in
    Alt (fmap f p) (fmap s gs)

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

--------------------------------------------------------------------
-- Serialization
--------------------------------------------------------------------

instance Serial HardTerm
instance Binary HardTerm where put = serialize ; get = deserialize
instance Serialize HardTerm where put = serialize ; get = deserialize

instance Serial BodyBound
instance Binary BodyBound where put = serialize ; get = deserialize
instance Serialize BodyBound where put = serialize ; get = deserialize

instance Serial WhereBound
instance Binary WhereBound where put = serialize ; get = deserialize
instance Serialize WhereBound where put = serialize ; get = deserialize

instance Serial1 BindingType where
  serializeWith f (Explicit a) = putWord8 0 >> f a
  serializeWith _ Implicit     = putWord8 1
  deserializeWith m = getWord8 >>= \a -> case a of
    0 -> Explicit <$> m
    1 -> return Implicit
    _ -> fail "BindingType.deserializeWith: unexpected case"

instance Serial t => Serial (BindingType t) where
  serialize = serialize1 ; deserialize = deserialize1

instance Binary t => Binary (BindingType t) where
  put = serializeWith Binary.put
  get = deserializeWith Binary.get

instance Serialize t => Serialize (BindingType t) where
  put = serializeWith Serialize.put
  get = deserializeWith Serialize.get

instance Serial2 Binding where
  serializeWith2 pt pa (Binding _ bt body) =
    serializeWith pt bt *> serializeWith (serializeWith2 pt pa) body

  deserializeWith2 gt ga = Binding <$> return mempty
                                   <*> deserializeWith gt
                                   <*> deserializeWith (deserializeWith2 gt ga)

instance Serial t => Serial1 (Binding t) where
  serializeWith = serializeWith2 serialize
  deserializeWith = deserializeWith2 deserialize

instance (Serial t, Serial v) => Serial (Binding t v) where
  serialize = serialize1 ; deserialize = deserialize1

instance (Binary t, Binary v) => Binary (Binding t v) where
  put = serializeWith2   Binary.put Binary.put
  get = deserializeWith2 Binary.get Binary.get

instance Serial2 Body where
  serializeWith2 pt pa (Body pats g bs) =
    serializeWith (serializeWith pt) pats *>
    serializeWith (serializeScope3 serialize (serializeWith2 pt) pa) g *>
    serializeWith (serializeWith2 pt (serializeWith pa)) bs

  deserializeWith2 gt ga =
    Body <$> deserializeWith (deserializeWith gt)
         <*> deserializeWith (deserializeScope3 deserialize (deserializeWith2 gt) ga)
         <*> deserializeWith (deserializeWith2 gt (deserializeWith ga))

instance Serial t => Serial1 (Body t) where
  serializeWith = serializeWith2 serialize

  deserializeWith = deserializeWith2 deserialize

instance (Serial t, Serial v) => Serial (Body t v) where
  serialize = serialize1 ; deserialize = deserialize1

instance (Binary t, Binary a) => Binary (Body t a) where
  put = serializeWith2   Binary.put Binary.put
  get = deserializeWith2 Binary.get Binary.get

instance (Serialize t, Serialize a) => Serialize (Body t a) where
  put = serializeWith2   Serialize.put Serialize.put
  get = deserializeWith2 Serialize.get Serialize.get

instance Serial2 Term where
  serializeWith2 pt pa = go
   where
   go (Var a)        = putWord8 0 *> pa a
   go (App t1 t2)    = putWord8 1 *> go t1 *> go t2
   go (HardTerm h)   = putWord8 2 *> serialize h
   go (Sig t1 t)     = putWord8 3 *> go t1 *> pt t
   go (Lam ps s)     =
     putWord8 4 *> serializeWith (serializeWith pt) ps
                *> serializeScope3 serialize (serializeWith2 pt) pa s
   go (Case t alts)  =
     putWord8 5 *> go t *> serializeWith (serializeAlt3 pt (serializeWith2 pt) pa) alts
   go (Let bs s)     =
     putWord8 6 *> serializeWith (serializeWith2 pt pa) bs
                *> serializeScope3 serialize (serializeWith2 pt) pa s
   go (Loc _ t)      = putWord8 7 *> go t
   go (Remember i t) = putWord8 8 *> serialize i *> go t
  {-# INLINE serializeWith2 #-}

  deserializeWith2 gt ga = go
   where
   go = getWord8 >>= \b -> case b of
     0 -> Var <$> ga
     1 -> App <$> go <*> go
     2 -> HardTerm <$> deserialize
     3 -> Sig  <$> go <*> gt
     4 -> Lam  <$> deserializeWith (deserializeWith gt)
               <*> deserializeScope3 deserialize (deserializeWith2 gt) ga
     5 -> Case <$> go <*> deserializeWith (deserializeAlt3 gt (deserializeWith2 gt) ga)
     6 -> Let  <$> deserializeWith (deserializeWith2 gt ga)
               <*> deserializeScope3 deserialize (deserializeWith2 gt) ga
     7 -> Loc  <$> return mempty <*> go
     8 -> Remember <$> deserialize <*> go
     _ -> fail $ "getTerm: Unexpected constructor code: " ++ show b
  {-# INLINE deserializeWith2 #-}

instance Serial t => Serial1 (Term t) where
  serializeWith   = serializeWith2   serialize
  deserializeWith = deserializeWith2 deserialize

instance (Serial t, Serial v) => Serial (Term t v) where
  serialize   = serialize1
  deserialize = deserialize1

instance (Binary t, Binary v) => Binary (Term t v) where
  put = serializeWith2 Binary.put Binary.put
  get = deserializeWith2 Binary.get Binary.get

instance (Serialize t, Serialize v) => Serialize (Term t v) where
  put = serializeWith2 Serialize.put Serialize.put
  get = deserializeWith2 Serialize.get Serialize.get

makeClassy ''Binding
makePrisms ''BindingType
