{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExtendedDefaultRules #-}

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
import Data.Monoid
import Ermine.Core.Module
import Ermine.Syntax
import Ermine.Syntax.Core
import Ermine.Syntax.Global
import Ermine.Syntax.Kind as K
import Ermine.Syntax.Literal
import Ermine.Syntax.Pattern
import Ermine.Syntax.Term as Term
import Ermine.Syntax.Type as Type
import Prelude.Extras
import Test.QuickCheck
import Test.QuickCheck.Function
import Test.QuickCheck.Instances
import Test.Framework.TH
import Test.Framework.Providers.QuickCheck2

import Arbitrary

prop_pack_unpack_fixity f = (unpackFixity . packFixity) f == Just f

pack_unpack :: (Binary a, Eq a) => a -> Bool
pack_unpack a = runGet get (runPut $ put a) == a

prop_pack_unpack_global :: Global -> Bool
prop_pack_unpack_global = pack_unpack

prop_pack_unpack_hardkind :: HardKind -> Bool
prop_pack_unpack_hardkind = pack_unpack

prop_pack_unpack_kind :: Kind Int -> Bool
prop_pack_unpack_kind = pack_unpack

prop_pack_unpack_schema :: Schema Int -> Bool
prop_pack_unpack_schema = pack_unpack

prop_pack_unpack_hardtype :: HardType -> Bool
prop_pack_unpack_hardtype = pack_unpack

prop_pack_unpack_type :: Type Int Int -> Bool
prop_pack_unpack_type = pack_unpack

prop_pack_unpack_literal :: Literal -> Bool
prop_pack_unpack_literal = pack_unpack

prop_pack_unpack_hardterm :: HardTerm -> Bool
prop_pack_unpack_hardterm = pack_unpack

prop_pack_unpack_pattern :: Pattern Int -> Bool
prop_pack_unpack_pattern = pack_unpack

prop_pack_unpack_binding_type :: BindingType Int -> Bool
prop_pack_unpack_binding_type = pack_unpack

prop_pack_unpack_body_bound :: BodyBound -> Bool
prop_pack_unpack_body_bound = pack_unpack

prop_pack_unpack_where_bound :: WhereBound -> Bool
prop_pack_unpack_where_bound = pack_unpack

prop_pack_unpack_guarded :: Guarded Int -> Bool
prop_pack_unpack_guarded = pack_unpack

prop_pack_unpack_body :: Body Int Int -> Bool
prop_pack_unpack_body = pack_unpack

prop_pack_unpack_binding :: Binding Int Int -> Bool
prop_pack_unpack_binding = pack_unpack

prop_pack_unpack_term :: Term Int Int -> Bool
prop_pack_unpack_term = pack_unpack

prop_pack_unpack_hardcore :: HardCore -> Bool
prop_pack_unpack_hardcore = pack_unpack

prop_pack_unpack_core :: Core () Int -> Bool
prop_pack_unpack_core = pack_unpack

prop_pack_unpack_module :: Module Int -> Bool
prop_pack_unpack_module = pack_unpack

-- Random choice of Binary test case type. May or may not be worth
-- keeping this around.
data AnyBinary where
  AB :: (Show t, Eq t, Binary t) => t -> AnyBinary

binaryGens :: [Gen AnyBinary]
binaryGens = [ AB <$> (arbitrary :: Gen Global)
             , AB <$> (arbitrary :: Gen HardKind)
             , AB <$> (arbitrary :: Gen HardType)
             , AB <$> (arbitrary :: Gen (Kind Int))
             , AB <$> (arbitrary :: Gen (Schema Int))
             , AB <$> (arbitrary :: Gen (Type Int Int))
             , AB <$> (arbitrary :: Gen Literal)
             , AB <$> (arbitrary :: Gen HardTerm)
             , AB <$> (arbitrary :: Gen (Pattern Int))
             ]

instance Arbitrary AnyBinary where
  arbitrary = oneof binaryGens

instance Show AnyBinary where
  show (AB v) = "AB" ++ show v

prop_pack_unpack_random :: AnyBinary -> Bool
prop_pack_unpack_random (AB b) = pack_unpack b

tests = $testGroupGenerator

