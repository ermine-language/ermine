{-# LANGUAGE TemplateHaskell #-}
module Parser2 (tests) where

import Arbitrary
import Control.Applicative
import Control.Lens hiding (elements)
import Control.Monad
import Data.Bifunctor
import Data.Char
import Data.Hashable
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.List.NonEmpty as Nel
import Data.Either (isLeft)
import Debug.Trace
import Ermine.Parser.Module (fetchGraphM, topSort)
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

unionGraphs :: ModuleGraph -> ModuleGraph -> ModuleGraph
unionGraphs = HM.unionWith HS.union

addDep :: (Int, Int) -> ModuleGraph -> ModuleGraph
addDep (i, d) = unionGraphs (singletonGraph i [d])

smallInt :: Gen Int
smallInt = elements [0..100]

rec :: [Int] -> Int -> Gen ModuleGraph
rec source d = smaller $ do
  nrDeps <- length <$> listOf smallInt
  let (ddeps, rest) = splitAt nrDeps source
  gs    <- traverse (rec rest) ddeps
  return $ foldl unionGraphs (singletonGraph d ddeps) gs

possiblyCyclic :: Gen (Int, ModuleGraph)
possiblyCyclic = do 
  root   <- smallInt
  source <- infiniteListOf smallInt
  graph  <- rec source root
  return (root,graph)

acyclic :: Gen (Int, ModuleGraph)
acyclic = do graph <- rec [1..] 0; return (0, graph)

cyclic :: Gen (Int, ModuleGraph)
cyclic = do (i, g) <- possiblyCyclic; g' <- injectCycle g; return $ (i, g')

injectCycle :: ModuleGraph -> Gen ModuleGraph
injectCycle g = do 
  ks <- infiniteListOf (elements $ HM.keys g)
  let cycl = zip ks (takeWhile (head ks /=) (tail ks) ++ [head ks])
  return $ foldl (flip addDep) g cycl

isGraphHMIdentity :: (Int, ModuleGraph) -> Bool
isGraphHMIdentity (root, g) = a == b where
	g' = fmap HS.toList g
	a  = HS.fromList $ HM.keys g
	b  = HS.fromList . HM.keys . runIdentity $
	  	 fetchGraphM (g' HM.!) (const . return) (join HM.singleton root)

topSortFindsCycle :: ModuleGraph -> Bool
topSortFindsCycle g = isLeft $ runTopSort g

--runTopSort :: ModuleGraph -> Either [Int] [[(Int, HS.HashSet Int)]]
runTopSort g = fmap (HS.fromList . fmap fst) <$>
  topSort (HM.fromList . fmap (join (,)) . HM.keys $ g) (g' HM.!) 
  where g' = fmap HS.toList g

prop_isGraphHMIdentity_possibly_cyclic   :: Property
prop_isGraphHMIdentity_possibly_cyclic   = forAll possiblyCyclic isGraphHMIdentity

prop_isGraphHMIdentity_impossibly_cyclic :: Property
prop_isGraphHMIdentity_impossibly_cyclic = forAll acyclic isGraphHMIdentity

prop_isGraphHMIdentity_definitely_cyclic :: Property
prop_isGraphHMIdentity_definitely_cyclic = forAll cyclic  isGraphHMIdentity

-- Test that fetchGraphM is fine with circular deps, 
-- but then topSort detects and fails on a circle for the same data
prop_topSort_detects_cycles :: Property
prop_topSort_detects_cycles = 
  forAll cyclic  (\(i, g) -> isGraphHMIdentity (i,g) && topSortFindsCycle g)

prop_topSort_detects_no_cycles :: Property
prop_topSort_detects_no_cycles = 
  forAll acyclic (\(i, g) -> isGraphHMIdentity (i,g) && not (topSortFindsCycle g))

-- Invariant: Every element of the result is non-empty.
prop_topSort_elements_nonEmpty :: Property
prop_topSort_elements_nonEmpty = forAll 
  (suchThat possiblyCyclic (\(_,g) -> not $ topSortFindsCycle g)) 
  (\(i, g) -> either (const False) (all (not . HS.null)) (runTopSort g))

prop_fetchGraphM_nodeps_is_identity =
  let fgm = runIdentity . fetchGraphM (const []) undefined in
  \g -> fgm g == (g :: HM.HashMap Int Int)

prop_fetchGraphM_takes_one_step =
  \i g g' -> runIdentity (fetchGraphM (const (HM.keys g'))
                                      (const . return . (g' HM.!))
                                      (HM.insert i i g))
           == (HM.insert i i g `HM.union` g' :: HM.HashMap Int Int)

tests = $testGroupGenerator
