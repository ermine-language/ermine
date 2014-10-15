{-# LANGUAGE TemplateHaskell #-}
module Loader (tests) where

import Control.Lens
import Data.Void (Void)
import Ermine.Loader.Core
import Test.QuickCheck
import Test.QuickCheck.Function
import Test.QuickCheck.Instances
import Test.Framework.TH
import Test.Framework.Providers.QuickCheck2

prop_liftable_identity = let l = uncachedLoader Identity in
  \a -> ((l ^. load) (a :: Int) == return ((), a))
        && ((l ^. reload) a () == return (return ((), a)))

prop_alwaysFail_fails = let l = alwaysFail :: Loader () Maybe Int Void in
  \a -> ((l ^. load) a == Nothing)
        && ((l ^. reload) a () == Nothing)

prop_thenM_right_identity = let l = uncachedLoader Identity `thenM` return in
  \a -> ((l ^. load) (a :: Int) == return ((), a))
        && ((l ^. reload) a () == return (return ((), a)))

prop_thenM_preserves_reload =
  let l = Loader (const . Right $ ((), 42)) (const . const $ Right Nothing)
          `thenM` (\a -> Left a `asTypeOf` Right a)
  in \a -> (l ^. reload) (a :: Int) () == return Nothing

tests = $testGroupGenerator
