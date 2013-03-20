{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Test.QuickCheck
import Test.QuickCheck.Function
import Test.QuickCheck.Instances
import Test.Framework.TH
import Test.Framework.Providers.QuickCheck2

import VarProperties
import BinaryProperties

-- from VarProperties
prop_var_list  = prop_var_list_
prop_var_maybe = prop_var_maybe_

-- from BinaryProperties
prop_pack_unpack_fixity = prop_pack_unpack_fixity_
prop_pack_unpack_global = prop_pack_unpack_global_

main :: IO ()
main = $defaultMainGenerator
