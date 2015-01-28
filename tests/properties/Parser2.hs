{-# LANGUAGE TemplateHaskell #-}
module Parser2 (tests) where

import Arbitrary
import Control.Applicative
import Control.Lens
import Control.Monad
import Data.Bifunctor
import Data.Char
import Data.Hashable
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.List.NonEmpty as Nel
import Debug.Trace
import Ermine.Parser.Module (fetchGraphM)
import Test.QuickCheck
import Test.QuickCheck.Function
import Test.QuickCheck.Instances
import Test.Framework.TH
import Test.Framework.Providers.QuickCheck2

genNel :: Gen a -> Gen (Nel.NonEmpty a)
genNel g = (Nel.:|) <$> g <*> listOf g

type ModuleGraph = HM.HashMap Int (HS.HashSet Int) 

singletonGraph :: Int -> [Int] -> ModuleGraph
singletonGraph i ds = HM.singleton i (HS.fromList ds)

smallInt :: Gen Int
smallInt = oneof (map return [0..100])

arbModuleGraph :: Gen ModuleGraph
arbModuleGraph = sized f where
  f 0 = return HM.empty
  f n = smallInt >>= rec

rec :: Int -> Gen ModuleGraph
rec d = smaller $ do
  ddeps <- listOf smallInt
  gs    <- traverse rec ddeps
  return $ foldl (HM.unionWith HS.union) (singletonGraph d ddeps) gs

data CyclicGraph = CyclicGraph Int ModuleGraph deriving Show
instance Arbitrary CyclicGraph where
  arbitrary = do r <- smallInt; rec r >>= return . CyclicGraph r

prop_fetchGraphHMIdentity :: CyclicGraph -> Bool
prop_fetchGraphHMIdentity (CyclicGraph root g) = 
  trace (show g) (a == b) where
	g' = fmap HS.toList g
	a  = HS.fromList (HM.keys g)
	b  = HS.fromList . HM.keys . runIdentity $
	  	 fetchGraphM (g' HM.!) (const . return) (join HM.singleton root)

prop_fetchGraphM_nodeps_is_identity =
  let fgm = runIdentity . fetchGraphM (const []) undefined in
  \g -> fgm g == (g :: HM.HashMap Int Int)

prop_fetchGraphM_takes_one_step =
  \i g g' -> runIdentity (fetchGraphM (const (HM.keys g'))
                                      (const . return . (g' HM.!))
                                      (HM.insert i i g))
           == (HM.insert i i g `HM.union` g' :: HM.HashMap Int Int)

tests = $testGroupGenerator
