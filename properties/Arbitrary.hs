{-# LANGUAGE DefaultSignatures #-}

module Arbitrary where

import Bound
import Control.Applicative
import Data.Monoid
import Ermine.Syntax.Core    as Core
import Ermine.Syntax.Global  as Global
import Ermine.Syntax.Kind    as Kind
import Ermine.Syntax.Literal as Lit
import Ermine.Syntax.Pattern as Pat
import Ermine.Syntax.Type    as Type
import Ermine.Syntax.Term    as Term
import Prelude.Extras
import Test.QuickCheck
import Test.QuickCheck.Function
import Test.QuickCheck.Instances

-- Orphans
instance Arbitrary HardKind where
  arbitrary = oneof $ return <$> [ Star, Constraint, Rho, Phi ]

instance Arbitrary a => Arbitrary (Kind a) where
  arbitrary = oneof [
    Kind.Var <$> arbitrary,
    (:->)    <$> arbitrary <*> arbitrary,
    HardKind <$> arbitrary ]

instance Arbitrary Assoc where
  arbitrary = Test.QuickCheck.elements [L, R, N]

genPrecedence = choose (0, 9) :: Gen Int

instance Arbitrary Fixity where
  arbitrary = oneof [
    Infix   <$> arbitrary <*> genPrecedence,
    Prefix  <$> genPrecedence,
    Postfix <$> genPrecedence,
    return  Idfix ]

instance Arbitrary Global where
  arbitrary = glob <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (Arbitrary a, Arbitrary b) => Arbitrary (Var a b) where
  arbitrary = oneof [ B <$> arbitrary, F <$> arbitrary ]

instance (Arbitrary b,Arbitrary v,Arbitrary1 f,Functor f) => Arbitrary (Scope b f v) where
  arbitrary = Scope . fmap (fmap lower1) <$> arbitrary1

instance Arbitrary a => Arbitrary (Schema a) where
  arbitrary = Schema <$> arbitrary <*> arbitrary

instance Arbitrary Type.HardType where
  arbitrary = oneof [
    Type.Tuple  <$> arbitrary,
    return Type.Arrow,
    -- TODO: learn more about Data.Void
    -- | Con !Global !(Schema Void)
    --Type.Con    <$> arbitrary <*> arbitrary,
    Type.ConcreteRho <$> arbitrary ]

-- used in Type arbitrary. 
-- i couldn't seem to put it in a where clause. -JC 3/22/13
resizearb :: Arbitrary a => Int -> Gen a
resizearb n = resize (n `div` 2) arbitrary

instance (Arbitrary k, Arbitrary a) => Arbitrary (Type k a) where
  arbitrary = sized type' where
    type' 0 = oneof [ Type.Var <$> arbitrary, HardType <$> arbitrary ]
    type' n | n>0 = oneof [
      Type.Var    <$> arbitrary
     ,Type.App      <$> resizearb n <*> resizearb n
     ,HardType <$> arbitrary
     ,Forall   <$> arbitrary <*> resizearb n <*> resizearb n <*> resizearb n
     -- | Loc !Rendering !(Type k a)
     --,Loc something something
     ,Exists   <$> arbitrary <*> resizearb n <*> resizearb n
     ,And      <$> resizearb n ]

instance Arbitrary Literal where
  arbitrary = oneof [
    Int     <$> arbitrary,
    Int64   <$> arbitrary,
    Byte    <$> arbitrary,
    Short   <$> arbitrary,
    String  <$> arbitrary,
    Char    <$> arbitrary,
    Float   <$> arbitrary,
    Double  <$> arbitrary ]

instance Arbitrary t => Arbitrary (Pattern t) where
  arbitrary = sized tree' where
    tree' 0 =  oneof [ SigP <$> arbitrary, return WildcardP, LitP <$> arbitrary ]
    tree' n = oneof [
      SigP    <$> arbitrary,
      return WildcardP,
      AsP     <$> resizearb n,
      StrictP <$> resizearb n,
      LazyP   <$> resizearb n,
      LitP    <$> arbitrary,
      ConP    <$> arbitrary <*> resizearb n,
      TupP    <$> resizearb n ]

instance (Arbitrary t,Arbitrary1 f,Arbitrary a,Functor f) => Arbitrary (Alt t f a) where
  arbitrary = Alt <$> arbitrary <*> arbitrary

instance Arbitrary t => Arbitrary (BindingType t) where
  arbitrary = oneof [ Explicit <$> arbitrary, return Implicit ]

instance Arbitrary DeclBound where
  arbitrary = oneof [ D <$> arbitrary, P <$> arbitrary, W <$> arbitrary ]

instance Arbitrary tm => Arbitrary (Guarded tm) where
  arbitrary = oneof [ Unguarded <$> arbitrary, Guarded <$> arbitrary ]

instance (Arbitrary t, Arbitrary a) => Arbitrary (Body t a) where
  arbitrary = sized body' where
    body' 0 = Body <$> return [] <*> resizearb 0 <*> return []
    body' n = Body <$> arbitrary <*> resizearb n <*> resizearb n

instance (Arbitrary t, Arbitrary a) => Arbitrary (Binding t a) where
  arbitrary = sized binding' where
    binding' 0 = Binding <$> return mempty <*> arbitrary <*> return []
    binding' n = Binding <$> return mempty <*> arbitrary <*> resizearb n

instance Arbitrary HardTerm where
  arbitrary = oneof [
    Term.Lit     <$> arbitrary,
    DataCon      <$> arbitrary,
    Term.Tuple   <$> arbitrary,
    return Hole ]

instance (Arbitrary t, Arbitrary a) => Arbitrary (Term t a) where
  arbitrary = sized term' where
    term' 0 = oneof [ Term.Var <$> arbitrary, HardTerm <$> arbitrary ]
    term' n = oneof [
      Term.Var  <$> arbitrary,
      Term.App  <$> resizearb n <*> resizearb n,
      HardTerm  <$> arbitrary,
      Term.Sig  <$> resizearb n <*> arbitrary,
      Term.Lam  <$> arbitrary <*> resizearb n,
      Term.Case <$> resizearb n <*> resizearb n,
      Term.Let  <$> resizearb n <*> resizearb n,
      Term.Loc  <$> return mempty <*> resizearb n,
      Term.Remember <$> arbitrary <*> resizearb n ]

instance Arbitrary a => Arbitrary (Branch a) where
  arbitrary = oneof [ Labeled <$> arbitrary <*> arbitrary, Default <$> arbitrary ]

instance Arbitrary HardCore where
  arbitrary = oneof [ Super  <$> arbitrary, Slot <$> arbitrary, Core.Lit <$> arbitrary ]

instance Arbitrary a => Arbitrary (Core a) where
  arbitrary = sized core' where
    core' 0 = oneof [ Core.Var <$> arbitrary, HardCore <$> arbitrary ]
    core' n = oneof [
      Core.Var  <$> arbitrary,
      HardCore  <$> arbitrary,
      Core.App  <$> resizearb n <*> resizearb n,
      Core.Lam  <$> arbitrary   <*> resizearb n,
      Core.Let  <$> resizearb n <*> resizearb n,
      Core.Case <$> resizearb n <*> resizearb n,
      Core.Dict <$> resizearb n <*> resizearb n,
      Core.LamDict <$> resizearb n,
      Core.AppDict <$> resizearb n <*> resizearb n ]

-- Higher-order arbitrary
class Arbitrary1 f where
  arbitrary1 :: Arbitrary a => Gen (f a)
  default arbitrary1 :: Arbitrary (f a) => Gen (f a)
  arbitrary1 = arbitrary

instance Arbitrary a => Arbitrary1 ((,) a)
instance Arbitrary1 Maybe
instance Arbitrary1 []
instance Arbitrary a => Arbitrary1 (Either a)
instance Arbitrary1 Kind
instance Arbitrary1 Core
instance Arbitrary k => Arbitrary1 (Type k)
instance Arbitrary t => Arbitrary1 (Term t)

instance (Arbitrary1 f, Arbitrary u) => Arbitrary (Lift1 f u) where
  arbitrary = Lift1 <$> arbitrary1


