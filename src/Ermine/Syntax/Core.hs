{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
--------------------------------------------------------------------
-- |
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
  , HardCore(..)
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
import Data.Binary as Binary
import Data.Data
import Data.Int
import Data.List as List
import Data.Foldable
import Data.Hashable
import Data.Hashable.Extras
import Data.String
import Ermine.Syntax
import Ermine.Syntax.Binary
import Ermine.Syntax.Literal
import Ermine.Syntax.Scope
import GHC.Generics
import Prelude.Extras
import Prelude

-- $setup
-- >>> import Text.Groom

-- | The built-in '::' constructor for a list.
--
-- >>> putStrLn $ groom $ lit (1 :: Int) `cons` nil
-- Data 1 [HardCore (Lit (Int 1)), Data 0 []]
instance (Choice p, Applicative f) => Cons p f (Core a) (Core a) (Core a) (Core a) where
  _Cons = prism (\(a, as) -> Data 1 [a,as]) $ \ s -> case s of
    Data 1 [x,xs] -> Right (x,xs)
    _             -> Left s

-- | The built-in '[]' constructor for a list.
nil :: Core a
nil = Data 0 []

-- | The built-in 'Just' constructor for 'Maybe'.
just :: Core a -> Core a
just a = Data 1 [a]

-- | The built-in 'Nothing' constructor for 'Maybe'.
nothing :: Core a
nothing = Data 0 []

-- | Lifting of literal values to core.
class Lit a where
  lit  :: a   -> Core b
  lits :: [a] -> Core b
  lits = Prelude.foldr (Lens.cons . lit) nil

instance Lit Int64 where lit l = HardCore . Lit $ Int64 l
instance Lit Int where lit i = HardCore . Lit $ Int i
instance Lit Char where
  lit c = HardCore . Lit $ Char c
  lits s = HardCore . Lit $ String s
instance Lit Int8 where lit b = HardCore . Lit $ Byte b
instance Lit Int16 where lit s = HardCore . Lit $ Short s
instance (Lit a, Lit b) => Lit (a, b) where
  lit (a,b) = Data 0 [lit a, lit b]
instance Lit a => Lit [a] where
  lit = lits
instance Lit a => Lit (Maybe a) where
  lit = maybe nothing (just . lit)

data Branch a = Labeled { tag :: !Int, body :: Scope Int Core a }
              | Default {              body :: Scope Int Core a }
  deriving (Eq,Show,Read,Functor,Foldable,Traversable,Generic)

instance Hashable1 Branch
instance Hashable a => Hashable (Branch a)

instance BoundBy Branch Core where
  boundBy f (Default   b) = Default   (boundBy f b)
  boundBy f (Labeled n b) = Labeled n (boundBy f b)

instance Binary a => Binary (Branch a) where
  put = putBranch put
  get = getBranch get

putBranch :: (a -> Put) -> Branch a -> Put
putBranch pa (Labeled i b) = putWord8 0 *> put i *> putScope put putCore pa b
putBranch pa (Default   b) = putWord8 1 *>          putScope put putCore pa b

getBranch :: Get a -> Get (Branch a)
getBranch ga = getWord8 >>= \b -> case b of
  0 -> Labeled <$> get <*> getScope get getCore ga
  1 -> Default <$>         getScope get getCore ga
  _ -> fail $ "getBranch: Unexpected constructor code: " ++ show b

data HardCore
  = Super   !Int
  | Slot    !Int
  | Lit     !Literal
  deriving (Eq,Ord,Show,Read,Data,Typeable,Generic)

instance Hashable HardCore

instance Binary HardCore where
  put (Super i) = putWord8 0 *> put i
  put (Slot g)  = putWord8 1 *> put g
  put (Lit i)   = putWord8 2 *> put i

  get = getWord8 >>= \b -> case b of
    0 -> Super <$> get
    1 -> Slot  <$> get
    2 -> Lit   <$> get
    _ -> fail   $ "get HardCore: Unexpected constructor code: " ++ show b

-- | Core values are the output of the compilation process.
--
-- They are terms where the dictionary passing has been made explicit
-- and all of the types have been checked and removed.
data Core a
  = Var a
  | HardCore !HardCore
  | Data !Int [Core a]
  | App !(Core a) !(Core a)
  | Lam !Int !(Scope Int Core a)
  | Let [Scope Int Core a] !(Scope Int Core a)
  | Case !(Core a) [Branch a] -- TODO: IntMap?
  | Dict { supers :: [Core a], slots :: [Scope Int Core a] }
  | LamDict !(Scope () Core a)
  | AppDict !(Core a) !(Core a)
  deriving (Eq,Show,Read,Functor,Foldable,Traversable)

-- | Binary serialization of a 'Core', given serializers for its parameter.
putCore :: (a -> Put) -> Core a -> Put
putCore pa (Var a)          = putWord8 0 *> pa a
putCore _  (HardCore h)     = putWord8 1 *> put h
putCore pa (Data i cs)      = putWord8 2 *> put i *> putMany (putCore pa) cs
putCore pa (App c1 c2)      = putWord8 3 *> putCore pa c1 *> putCore pa c2
putCore pa (Lam i s)        = putWord8 4 *> put i *> putScope put putCore pa s
putCore pa (Let ss s)       = putWord8 5 *> putMany (putScope put putCore pa) ss *> putScope put putCore pa s
putCore pa (Case c bs)      = putWord8 6 *> putCore pa c *> putMany (putBranch pa) bs
putCore pa (Dict sups slts) = putWord8 7 *> putMany (putCore pa) sups *> putMany (putScope put putCore pa) slts
putCore pa (LamDict s)      = putWord8 8 *> putScope put putCore pa s
putCore pa (AppDict c1 c2)  = putWord8 9 *> putCore pa c1 *> putCore pa c2

getCore :: Get a -> Get (Core a)
getCore ga = getWord8 >>= \b -> case b of
  0 -> Var      <$> ga
  1 -> HardCore <$> get
  2 -> Data     <$> get <*> getMany (getCore ga)
  3 -> App      <$> getCore ga <*> getCore ga
  4 -> Lam      <$> get <*> getScope get getCore ga
  5 -> Let      <$> getMany (getScope get getCore ga) <*> getScope get getCore ga
  6 -> Case     <$> getCore ga <*> getMany (getBranch ga)
  7 -> Dict     <$> getMany (getCore ga) <*> getMany (getScope get getCore ga)
  8 -> LamDict  <$> getScope get getCore ga
  9 -> AppDict  <$> getCore ga <*> getCore ga
  _ -> fail $ "getCore: Unexpected constructor code: " ++ show b

instance Binary a => Binary (Core a) where
  put = putCore put
  get = getCore get

instance Hashable1 Core

distHardCore, distData, distApp, distLam, distLet, distCase, distDict, distLamDict, distAppDict :: Word
distHardCore = maxBound `quot` 3
distData     = maxBound `quot` 5
distApp      = maxBound `quot` 7
distLam      = maxBound `quot` 11
distLet      = maxBound `quot` 13
distCase     = maxBound `quot` 17
distDict     = maxBound `quot` 19
distLamDict  = maxBound `quot` 23
distAppDict  = maxBound `quot` 29

instance Hashable a => Hashable (Core a) where
  hashWithSalt n (Var a)       = hashWithSalt n a
  hashWithSalt n (HardCore c)  = hashWithSalt n c  `hashWithSalt` distHardCore
  hashWithSalt n (Data i cs)   = hashWithSalt n i  `hashWithSalt` cs `hashWithSalt` distData
  hashWithSalt n (App x y)     = hashWithSalt n x  `hashWithSalt` y `hashWithSalt` distApp
  hashWithSalt n (Lam k b)     = hashWithSalt n k  `hashWithSalt` b `hashWithSalt` distLam
  hashWithSalt n (Let ts b)    = hashWithSalt n ts `hashWithSalt` b `hashWithSalt` distLet
  hashWithSalt n (Case c bs)   = hashWithSalt n c  `hashWithSalt` bs `hashWithSalt` distCase
  hashWithSalt n (Dict s ss)   = hashWithSalt n s  `hashWithSalt` ss `hashWithSalt` distDict
  hashWithSalt n (LamDict b)   = hashWithSalt n b `hashWithSalt` distLamDict
  hashWithSalt n (AppDict x y) = hashWithSalt n x `hashWithSalt` y `hashWithSalt` distAppDict

instance IsString a => IsString (Core a) where
  fromString = Var . fromString

instance App (Core a) where
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
  HardCore h  >>= _ = HardCore h
  Data n xs   >>= f = Data n ((>>= f) <$> xs)
  App x y     >>= f = App (x >>= f) (y >>= f)
  Lam n e     >>= f = Lam n (boundBy f e)
  Let bs e    >>= f = Let (boundBy f <$> bs) (boundBy f e)
  Case e as   >>= f = Case (e >>= f) (boundBy f <$> as)
  Dict xs ys  >>= f = Dict ((>>= f) <$> xs) ((>>>= f) <$> ys)
  LamDict e   >>= f = LamDict (e >>>= f)
  AppDict x y >>= f = AppDict (x >>= f) (y >>= f)

instance Eq1 Core
instance Show1 Core
instance Read1 Core

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
