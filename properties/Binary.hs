{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Binary where

import Bound.Scope
import Bound.Var
import Control.Monad
import Control.Applicative
import Control.Lens
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.ByteString
import Ermine.Syntax
import Ermine.Syntax.Global
import Ermine.Syntax.Kind as K
import Ermine.Syntax.Type as T
import Prelude.Extras
import Test.QuickCheck
import Test.QuickCheck.Function
import Test.QuickCheck.Instances
import Test.Framework.TH
import Test.Framework.Providers.QuickCheck2

class Arbitrary1 f where arbitrary1 :: Arbitrary a => Gen (f a)

instance Arbitrary Assoc where
  arbitrary     = Test.QuickCheck.elements [L, R, N]

genPrecedence = choose (0, 9) :: Gen Int

instance Arbitrary Fixity where
    arbitrary =
      oneof [ Infix   <$> arbitrary <*> genPrecedence,
              Prefix  <$> genPrecedence,
              Postfix <$> genPrecedence,
              return  Idfix ]

prop_pack_unpack_fixity f = (unpackFixity . packFixity) f == f

instance Arbitrary Global where
    arbitrary = global <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

pack_unpack :: (Binary a, Eq a) => a -> Bool
pack_unpack a = runGet get (runPut $ put a) == a

prop_pack_unpack_global :: Global -> Bool
prop_pack_unpack_global = pack_unpack

instance Arbitrary HardKind where
    arbitrary = oneof $ return <$> [ Star, Constraint, Rho, Phi ]

prop_pack_unpack_hardkind :: HardKind -> Bool
prop_pack_unpack_hardkind = pack_unpack

instance Arbitrary a => Arbitrary (Kind a) where
    arbitrary =
      oneof [ K.Var    <$> arbitrary,
              (:->)    <$> arbitrary <*> arbitrary,
              HardKind <$> arbitrary ]

instance Arbitrary1 Kind where
    arbitrary1 = arbitrary

prop_pack_unpack_kind :: Kind Int -> Bool
prop_pack_unpack_kind = pack_unpack

instance (Arbitrary a, Arbitrary b) => Arbitrary (Var a b) where
    arbitrary = oneof [ B <$> arbitrary, F <$> arbitrary ]

instance (Arbitrary1 f, Arbitrary u) => Arbitrary (Lift1 f u) where
    arbitrary = Lift1 <$> arbitrary1

instance (Arbitrary b,Arbitrary v,Arbitrary1 f,Functor f) => Arbitrary (Scope b f v) where
    arbitrary = Scope . fmap (fmap lower1) <$> arbitrary1

instance Arbitrary a => Arbitrary (Schema a) where
    arbitrary = Schema <$> arbitrary <*> arbitrary

prop_pack_unpack_schema :: Schema Int -> Bool
prop_pack_unpack_schema = pack_unpack

instance Arbitrary T.HardType where
    arbitrary = 
      oneof [ T.Tuple  <$> arbitrary,
              return T.Arrow,
              -- TODO: learn more about Data.Void
              -- | Con !Global !(Schema Void)
              --T.Con    <$> arbitrary <*> arbitrary,
              T.ConcreteRho <$> arbitrary ]

instance Arbitrary k => Arbitrary1 (Type k) where
    arbitrary1 = arbitrary 

resizearb :: Arbitrary a => Int -> Gen a
resizearb n = resize (n `div` 2) arbitrary

instance (Arbitrary k, Arbitrary a) => Arbitrary (Type k a) where
    arbitrary = sized type' where 
      type' 0 = oneof [ T.Var <$> arbitrary, HardType <$> arbitrary ]
      type' n | n>0 = 
        oneof [ T.Var    <$> arbitrary
               ,App      <$> resizearb n <*> resizearb n
               ,HardType <$> arbitrary
               ,Forall   <$> arbitrary <*> resizearb n <*> resizearb n <*> resizearb n
               -- | Loc !Rendering !(Type k a)
               --,Loc something something
               ,Exists   <$> arbitrary <*> resizearb n <*> resizearb n
               ,And      <$> resizearb n
              ]
        where subtype = type' (n `div` 2)

prop_pack_unpack_hardtype :: HardType -> Bool
prop_pack_unpack_hardtype = pack_unpack

prop_pack_unpack_type :: Type Int Int -> Bool
prop_pack_unpack_type = pack_unpack

tests = $testGroupGenerator

