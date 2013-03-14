{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, MultiParamTypeClasses, FlexibleInstances #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
--------------------------------------------------------------------
-- |
-- Module    :  Ermine.Syntax.Core
-- Copyright :  (c) Edward Kmett 2012
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--------------------------------------------------------------------

module Ermine.Syntax.Core
  (
  -- * Core Terms
    Branch(..)
  , Core(..)
  , Lit(..)
  -- * Smart constructors
  , lam
  , let_
  -- * Common built-in terms
  , cons
  , nil
  , just, nothing
  ) where

import Bound
import Control.Applicative
import Control.Monad
import Control.Lens as Lens
import Data.Int
import Data.List as List
import Data.Foldable
import Data.String
import Data.Vector as Vector hiding (cons, length)
import Ermine.Syntax
import Ermine.Syntax.Global
import Ermine.Syntax.Prim
import Ermine.Syntax.Scope
import Prelude.Extras
import Prelude

-- $setup
-- >>> import Text.Groom

-- | The built-in '::' constructor for a list.
--
-- >>> putStrLn $ groom $ lit (1 :: Int) `cons` nil
-- Prim (DataCon (global (Infix R 5) "ermine" "Builtin" "::"))
--   [Prim (Int 1) [],
--    Prim (DataCon (global Idfix "ermine" "Builtin" "Nil")) []]
instance (Choice p, Applicative f) => Cons p f (Core a) (Core a) (Core a) (Core a) where
  _Cons = prism (\(a, as) -> Prim consPrim [a,as]) $ \ s -> case s of
    Prim p [x,xs] | p == consPrim -> Right (x,xs)
    _                             -> Left s
    where consPrim = prim (Infix R 5) "Builtin" "::"

{-
instance Cons Reviewed Identity (Core a) (Core a) (Core a) (Core a) where
  _Cons = unto (\(a, as) -> Prim (prim (Infix R 5) "Builtin" "::") [a,as]
-}

-- | The built-in '[]' constructor for a list.
nil :: Core a
nil = Prim (prim Idfix "Builtin" "Nil") []

-- | The built-in 'Just' constructor for 'Maybe'.
just :: Core a -> Core a
just a = Prim (prim Idfix "Builtin" "Just") [a]

-- | The built-in 'Nothing' constructor for 'Maybe'.
nothing :: Core a
nothing = Prim (prim Idfix "Builtin" "Nothing") []

-- | Lifting of literal values to core.
class Lit a where
  lit  :: a   -> Core b
  lits :: [a] -> Core b
  lits = Prelude.foldr (Lens.cons . lit) nil

instance Lit Int64 where lit l = Prim (Int64 l) []
instance Lit Int where lit i = Prim (Int i) []
instance Lit Char where
  lit c = Prim (Char c) []
  lits s = Prim (String s) []
instance Lit Int8 where lit b = Prim (Byte b) []
instance Lit Int16 where lit s = Prim (Short s) []
instance (Lit a, Lit b) => Lit (a, b) where
  lit (a,b) = Prim (Tuple 2) [lit a, lit b]
instance Lit a => Lit [a] where
  lit = lits
instance Lit a => Lit (Maybe a) where
  lit = maybe nothing (just . lit)

instance Num (Core a) where
  a + b = Prim (prim (Infix L 6) "Builtin" "+") [a,b]
  a - b = Prim (prim (Infix L 6) "Builtin" "-") [a,b]
  a * b = Prim (prim (Infix L 7) "Builtin" "*") [a,b]
  negate a     = Prim (prim Idfix "Builtin" "negate") [a]
  abs a        = Prim (prim Idfix "Builtin" "abs")    [a]
  signum a     = Prim (prim Idfix "Builtin" "signum") [a]
  fromInteger i = Prim (Int (fromInteger i)) []

data Branch a = Branch { tag :: !Int, body :: Scope Int Core a }
  deriving (Eq,Show,Functor,Foldable,Traversable)

instance BoundBy Branch Core where
  boundBy f (Branch n b) = Branch n (boundBy f b)

-- | Core values are the output of the compilation process.
--
-- They are terms where the dictionary passing has been made explicit
-- and all of the types have been checked and removed.
data Core a
  = Var a
  | Prim !Prim [Core a]
  | App !(Core a) !(Core a)
  | Lam !Int !(Scope Int Core a)
  | Let [Scope Int Core a] !(Scope Int Core a)
  | Case !(Core a) (Scope () Core a) [Branch a] -- TODO: IntMap?
  | Dict { supers :: Vector (Core a), slots :: Vector (Scope Int Core a) }
  | LamDict !(Scope () Core a)
  | AppDict !(Core a) !(Core a)
  deriving (Eq,Show,Functor,Foldable,Traversable)

instance IsString a => IsString (Core a) where
  fromString = Var . fromString

instance App Core where
  app = prism (uncurry App) $ \t -> case t of
    App l r -> Right (l,r)
    _       -> Left t

instance Variable Core where
  var = prism Var $ \t -> case t of
    Var a -> Right a
    _     -> Left t

instance Applicative Core where
  pure = Var
  (<*>) = ap

instance Monad Core where
  return = Var
  Var a       >>= f = f a
  Prim k xs   >>= f = Prim k ((>>= f) <$> xs)
  App x y     >>= f = App (x >>= f) (y >>= f)
  Lam n e     >>= f = Lam n (boundBy f e)
  Let bs e    >>= f = Let (boundBy f <$> bs) (boundBy f e)
  Case e d as >>= f = Case (e >>= f) (d >>>= f) (boundBy f <$> as)
  Dict xs ys  >>= f = Dict ((>>= f) <$> xs) ((>>>= f) <$> ys)
  LamDict e   >>= f = LamDict (e >>>= f)
  AppDict x y >>= f = AppDict (x >>= f) (y >>= f)

instance Eq1 Core
instance Show1 Core

-- | smart lam constructor
lam :: Eq a => [a] -> Core a -> Core a
lam as t = Lam (length as) (abstract (`List.elemIndex` as) t)

-- | smart let constructor
let_ :: Eq a => [(a, Core a)] -> Core a -> Core a
let_ bs b = Let (abstr . snd <$> bs) (abstr b)
  where vs  = fst <$> bs
        abstr = abstract (`List.elemIndex` vs)

{-
letDict :: Eq a => Vector (a, Core a) -> Vector (a, Core a) -> Core a -> Core a
letDict supers vslots body = LamDict body' `AppDict` Dict (map snd supers) slots where
  where abstr = abstract (`elemIndex` vs)
     go a = case 
     body' = Scope $ body >>= \a -> case elemIndex supers a of
       Just i  -> Prim (Super i) (B ())
       Nothing -> case elemIndex vslots a of
         Just j -> Prim (Slot j) (B ())
         Nothing -> Var (F (Var a))
-}
