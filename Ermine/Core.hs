{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, DeriveDataTypeable #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
module Ermine.Core
  ( Core(..)
  , Assoc(..)
  , Fixity(..)
  , Con(..)
  , Lit(..)
  , Pat(..)
  , Alt(..)
  -- * Smart patterns
  , P
  , varp
  , wildp
  , asp
  , conp
  -- * Smart constructors
  , lam
  , let_
  , alt
  -- * Common built-in terms
  , cons, nil
  , just, nothing
  ) where

import Bound
import Control.Applicative
import Control.Monad
import Data.Int
import Data.List hiding (foldr)
import Data.Foldable
import Data.Traversable
import Data.Data hiding (Fixity, Infix)
import Prelude.Extras
import Prelude hiding (foldr)

data Assoc = L | R | N deriving (Eq,Ord,Show,Read,Enum,Data,Typeable)
data Fixity = Infix Assoc !Int | Prefix !Int | Postfix !Int | Idfix deriving (Eq,Ord,Show,Read,Data,Typeable)

data Con
  = Con Fixity String String
  | Product !Int
  | Int     !Int
  | Int64   !Int64
  | Byte    !Int8
  | Short   !Int16
  | String  String
  | Char    !Char
  | Float   !Float
  | Double  !Double
  deriving (Eq,Ord,Show,Read,Data,Typeable)

cons :: Core a -> Core a -> Core a
cons a as = Prim (Con (Infix R 5) "Builtin" ":") [a,as]

nil :: Core a
nil = Prim (Con Idfix "Builtin" "Nil") []

just :: Core a -> Core a
just a = Prim (Con Idfix "Builtin" "Just") [a]

nothing :: Core a
nothing = Prim (Con Idfix "Builtin" "Nothing") []

class Lit a where
  lit  :: a   -> Core b
  lits :: [a] -> Core b
  lits = foldr (cons . lit) nil

instance Lit Int64 where lit l = Prim (Int64 l) []
instance Lit Int where lit i = Prim (Int i) []
instance Lit Char where
  lit c = Prim (Char c) []
  lits s = Prim (String s) []
instance Lit Int8 where lit b = Prim (Byte b) []
instance Lit Int16 where lit s = Prim (Short s) []
instance (Lit a, Lit b) => Lit (a, b) where
  lit (a,b) = Prim (Product 2) [lit a, lit b]
instance Lit a => Lit [a] where
  lit = lits
instance Lit a => Lit (Maybe a) where
  lit = maybe nothing (just . lit)

data Core a
  = Var a
  | Prim Con [Core a]
  | Core a :@ Core a
  | Lam Pat (Scope Int Core a)
  | Let [Scope Int Core a] (Scope Int Core a)
  | Case (Core a) [Alt Core a]
  deriving (Eq,Ord,Show,Read,Functor,Foldable,Traversable)

instance Applicative Core where
  pure = Var
  (<*>) = ap

instance Monad Core where
  return = Var
  Var a      >>= f = f a
  Prim k xs  >>= f = Prim k (map (>>= f) xs)
  (x :@ y)   >>= f = (x >>= f) :@ (y >>= f)
  Lam p e    >>= f = Lam p (e >>>= f)
  Let bs e   >>= f = Let (map (>>>= f) bs) (e >>>= f)
  Case e as  >>= f = Case (e >>= f) (map (>>>= f) as)

instance Eq1   Core where (==#) = (==)
instance Ord1  Core where compare1 = compare
instance Show1 Core where showsPrec1 = showsPrec
instance Read1 Core where readsPrec1 = readsPrec

data Pat
  = VarP
  | WildP
  | AsP Pat
  | ConP Con [Pat]
  deriving (Eq,Ord,Show,Read)

data Alt f a = Alt Pat (Scope Int f a)
  deriving (Eq,Ord,Show,Read,Functor,Foldable,Traversable)

instance Bound Alt where
  Alt p b >>>= f = Alt p (b >>>= f)

-- ** smart patterns
data P a = P { pattern :: Pat, bindings :: [a] } deriving (Show,Read)

varp :: a -> P a
varp a = P VarP [a]

wildp :: P a
wildp = P WildP []

asp :: a -> P a -> P a
asp a (P p as) = P (AsP p) (a:as)

conp :: Con -> [P a] -> P a
conp g ps = P (ConP g (map pattern ps)) (ps >>= bindings)

-- | smart lam constructor
lam :: Eq a => P a -> Core a -> Core a
lam (P p as) t = Lam p (abstract (`elemIndex` as) t)

-- | smart let constructor
let_ :: Eq a => [(a, Core a)] -> Core a -> Core a
let_ bs b = Let (map (abstr . snd) bs) (abstr b)
  where vs  = map fst bs
        abstr = abstract (`elemIndex` vs)

-- | smart alt constructor
alt :: Eq a => P a -> Core a -> Alt Core a
alt (P p as) t = Alt p (abstract (`elemIndex` as) t)
