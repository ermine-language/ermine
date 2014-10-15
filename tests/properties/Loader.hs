{-# LANGUAGE TemplateHaskell #-}
module Loader (tests) where

import Control.Lens
import Ermine.Loader.Core
import Test.QuickCheck
import Test.QuickCheck.Function
import Test.QuickCheck.Instances
import Test.Framework.TH
import Test.Framework.Providers.QuickCheck2

prop_liftable_identity = let l = uncachedLoader Identity in
  \a -> ((l ^. load) (a :: Int) == return ((), a))
        && ((l ^. reload) a () == return (return ((), a)))

tests = $testGroupGenerator
