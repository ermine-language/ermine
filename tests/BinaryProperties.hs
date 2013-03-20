{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
module BinaryProperties where

import Control.Monad
import Control.Lens
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.ByteString
import Ermine.Syntax
import Ermine.Syntax.Global
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

instance Arbitrary Global where
    arbitrary = liftM4 global arbitrary arbitrary arbitrary arbitrary

prop_pack_unpack_fixity_ f = (unpackFixity . packFixity) f == f
prop_pack_unpack_global_ :: Global -> Bool
prop_pack_unpack_global_ g = runGet get (runPut $ put g) == g
