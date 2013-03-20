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
import Control.Lens
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.ByteString
import Ermine.Syntax
import Ermine.Syntax.Global
import Ermine.Syntax.Kind
import Test.QuickCheck
import Test.QuickCheck.Function
import Test.QuickCheck.Instances
import Test.Framework.TH
import Test.Framework.Providers.QuickCheck2

instance Arbitrary Assoc where
  arbitrary     = Test.QuickCheck.elements [L, R, N]

genPrecedence = choose (0, 9) :: Gen Int

instance Arbitrary Fixity where
    arbitrary =
      oneof [ liftM2 Infix   arbitrary genPrecedence,
              liftM  Prefix  genPrecedence,
              liftM  Postfix genPrecedence,
              return Idfix ]

prop_pack_unpack_fixity f = (unpackFixity . packFixity) f == f

instance Arbitrary Global where
    arbitrary = liftM4 global arbitrary arbitrary arbitrary arbitrary

pack_unpack :: (Binary a, Eq a) => a -> Bool
pack_unpack a = runGet get (runPut $ put a) == a

prop_pack_unpack_global :: Global -> Bool
prop_pack_unpack_global = pack_unpack

instance Arbitrary HardKind where
    arbitrary = oneof $ fmap return [ Star, Constraint, Rho, Phi ]

prop_pack_unpack_hardkind :: HardKind -> Bool
prop_pack_unpack_hardkind = pack_unpack

instance Arbitrary a => Arbitrary (Kind a) where
    arbitrary = 
      oneof [ liftM  Var arbitrary,
              liftM2 (:->) arbitrary arbitrary,
              liftM  HardKind arbitrary ]

prop_pack_unpack_kind :: Kind Int -> Bool
prop_pack_unpack_kind = pack_unpack

instance (Arbitrary a, Arbitrary b) => Arbitrary (Var a b) where
    arbitrary = oneof [ liftM B arbitrary, liftM F arbitrary ]

class Arbitrary1 f where arbitrary1 :: Arbitrary a => Gen (f a) 

newtype Fuck f u = Fuck (f u)

--instance (Arbitrary1 f, Arbitrary a) => Arbitrary (Fuck f a)

--instance (Arbitrary b, Arbitrary v, Arbitrary1 f) => Arbitrary (Scope b f v) where
--    arbitrary = liftM Scope arbitrary1

tests = $testGroupGenerator
