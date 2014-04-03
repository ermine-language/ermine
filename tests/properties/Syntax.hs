{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE TemplateHaskell #-}

module Syntax where

import Ermine.Syntax.Kind as Kind
import Ermine.Syntax.Type as Type
import Ermine.Syntax.Term as Term
import Test.QuickCheck
import Test.QuickCheck.Function
import Test.QuickCheck.Instances
import Test.Framework.TH
import Test.Framework.Providers.QuickCheck2

import Arbitrary

eq_reflexive :: Eq a => a -> Bool
eq_reflexive x = x == x

prop_kind_eq_reflexive :: Kind Int -> Bool
prop_kind_eq_reflexive = eq_reflexive

prop_type_eq_reflexive :: Type Int Int -> Bool
prop_type_eq_reflexive = eq_reflexive

prop_term_eq_reflexive :: Term Int Int -> Bool
prop_term_eq_reflexive = eq_reflexive

tests = $testGroupGenerator
