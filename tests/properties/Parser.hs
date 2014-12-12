{-# LANGUAGE TemplateHaskell #-}
module Parser (tests) where

import Control.Lens
import qualified Data.HashMap.Strict as HM
import Ermine.Parser.Module (fetchGraphM)
import Test.QuickCheck
import Test.QuickCheck.Function
import Test.QuickCheck.Instances
import Test.Framework.TH
import Test.Framework.Providers.QuickCheck2

prop_fetchGraphM_nodeps_is_identity =
  let fgm = runIdentity . fetchGraphM (const []) undefined in
  \g -> fgm g == (g :: HM.HashMap Int Int)

tests = $testGroupGenerator
