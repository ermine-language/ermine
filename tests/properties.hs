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
import Test.Framework
import Test.Framework.TH
import Test.Framework.Providers.QuickCheck2

import Properties.Var as Var
import Properties.Binary as Binary

main :: IO ()
main = defaultMain
  [ Var.tests
  , Binary.tests
  ]
