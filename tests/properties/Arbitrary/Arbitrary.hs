{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE RankNTypes #-}

module Arbitrary.Arbitrary
  ( Arbitrary1(..), smaller, maybeGen, genScope, genVar
  ) where

import Bound
import Control.Applicative
import Data.List.NonEmpty hiding (fromList)
import Data.Map
import Data.Monoid
import Data.Void
import Prelude.Extras
import Test.QuickCheck
import Test.QuickCheck.Function
import Test.QuickCheck.Instances

-- Orphans
instance (Arbitrary a, Arbitrary b) => Arbitrary (Var a b) where
  arbitrary = oneof [ B <$> arbitrary, F <$> arbitrary ]
instance (Arbitrary a) => Arbitrary1 (Var a) where
  liftArbitrary gb = oneof [ B <$> arbitrary, F <$> gb ]

instance (Arbitrary b,Arbitrary v,Arbitrary1 f,Functor f) => Arbitrary (Scope b f v) where
  arbitrary = liftArbitrary arbitrary

instance (Arbitrary b,Arbitrary1 f, Functor f) => Arbitrary1 (Scope b f) where
  liftArbitrary = fmap Scope . liftArbitrary . liftArbitrary . liftArbitrary

instance (Arbitrary1 f, Arbitrary u) => Arbitrary (Lift1 f u) where
  arbitrary = Lift1 <$> arbitrary1

genNel :: Gen a -> Gen (NonEmpty a)
genNel g = (:|) <$> g <*> listOf g

-- | Combinator for decreasing the size of a generator. Should be used when
-- generating tree structures, as relying on probability to terminate them
-- can lead to excessive memory consumption.
smaller :: Gen a -> Gen a
smaller g = sized $ \n -> resize (n`div`3) g

maybeGen :: Applicative f => Maybe (Gen a) -> [Gen (f a)]
maybeGen Nothing  = []
maybeGen (Just g) = [ pure <$> g ]

-- | Given a definite generator for bound varibles, and an optional one for
-- free variables, definitely generates Vars.
genVar :: Gen b -> Maybe (Gen a) -> Gen (Var b a)
genVar gb Nothing   = B <$> gb
genVar gb (Just ga) = oneof [ B <$> gb , F <$> ga ]

-- | As genVar, but allows for the possibility that bound variables cannot
-- be generated, either. Potentially useful for generating well-scoped
-- terms.
genVar' :: Maybe (Gen b) -> Maybe (Gen a) -> Maybe (Gen (Var b a))
genVar' (Just gb) mga       = Just $ genVar gb mga
genVar' Nothing   (Just ga) = Just (F <$> ga)
genVar' Nothing   Nothing   = Nothing

-- | Generates scopes with a definite supply of bound variables. The
-- higher-order generator must be able to handle a lack of free variables.
genScope :: Gen b -> (forall z. Maybe (Gen z) -> Gen (f z)) -> Maybe (Gen a)
         -> Gen (Scope b f a)
genScope gb gf mga = Scope <$> gf (Just . genVar gb . Just $ gf mga)

-- | As genScope, but with the possibility of no bound variables.
genScope' :: Maybe (Gen b) -> (forall z. Maybe (Gen z) -> Gen (f z)) -> Maybe (Gen a)
          -> Gen (Scope b f a)
genScope' mgb gf mga = Scope <$> gf (genVar' mgb . Just $ gf mga)
