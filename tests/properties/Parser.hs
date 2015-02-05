{-# LANGUAGE TemplateHaskell #-}
module Parser (tests) where

import Arbitrary
import Control.Applicative
import Control.Lens hiding (elements)
import Control.Monad
import Data.Bifunctor
import Data.Char
import Data.Hashable
import qualified Data.List.NonEmpty as Nel
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.Either (isLeft)
import Debug.Trace
import Ermine.Parser.Module (fetchGraphM, topSort)
import Test.QuickCheck
import Test.QuickCheck.Function
import Test.QuickCheck.Instances
import Test.Framework.TH
import Test.Framework.Providers.QuickCheck2

type ModuleName = String

arbModuleName :: Gen ModuleName
arbModuleName = nelToMName <$> genNel modPart where
  nelToMName  = concat . Nel.toList . Nel.intersperse "."
  modPart     = fmap (take 10) $ (:) <$> upperAlpha <*> listOf lowerAlpha
  letters     = "abcdefghijklmnopqrstuvwxyz"
  lowerAlpha  = oneof (map return letters)
  upperAlpha  = oneof (map (return . toUpper) letters)

type ModuleGraph a = HM.HashMap a (HS.HashSet a) 

singletonGraph :: (Eq a, Hashable a) => a -> [a] -> ModuleGraph a
singletonGraph a deps = HM.singleton a (HS.fromList deps)

unionGraphs :: (Eq a, Hashable a) => ModuleGraph a -> ModuleGraph a -> ModuleGraph a
unionGraphs = HM.unionWith HS.union

addDep :: (Eq a, Hashable a) => (a, a) -> ModuleGraph a -> ModuleGraph a
addDep (a, dep) = unionGraphs (singletonGraph a [dep])

smallInt :: Gen Int
smallInt = elements [0..100]

rec :: (Eq a, Hashable a) => [a] -> a -> Gen (ModuleGraph a)
rec source d = smaller $ do
  nrDeps <- length <$> listOf smallInt
  let (ddeps, rest) = splitAt nrDeps source
  gs    <- traverse (rec rest) ddeps
  return $ foldl unionGraphs (singletonGraph d ddeps) gs

possiblyCyclic :: (Eq a, Hashable a) => Gen a -> Gen (a, ModuleGraph a)
possiblyCyclic g = do 
  root   <- g
  source <- infiniteListOf g
  graph  <- rec source root
  return (root,graph)

acyclic :: Gen (Int, ModuleGraph Int)
acyclic = do graph <- rec [1..] 0; return (0, graph)

cyclic :: (Eq a, Hashable a) => Gen a -> Gen (a, ModuleGraph a)
cyclic gen = do 
  (i, g) <- possiblyCyclic gen 
  g' <- injectCycle g
  return $ (i, g')

injectCycle :: (Eq a, Hashable a) => ModuleGraph a -> Gen (ModuleGraph a)
injectCycle g = do 
  ks <- infiniteListOf (elements $ HM.keys g)
  let cycl = zip ks (takeWhile (head ks /=) (tail ks) ++ [head ks])
  return $ foldl (flip addDep) g cycl

isGraphHMIdentity :: (Eq a, Hashable a) => (a, ModuleGraph a) -> Bool
isGraphHMIdentity (root, g) = a == b where
	g' = fmap HS.toList g
	a  = HS.fromList $ HM.keys g
	b  = HS.fromList . HM.keys . runIdentity $
	  	 fetchGraphM (g' HM.!) (const . return) (join HM.singleton root)

topSortFindsCycle :: (Eq a, Hashable a) => ModuleGraph a -> Bool
topSortFindsCycle g = isLeft $ runTopSort g

idAndTopSortFindsCycle :: (Eq a, Hashable a) => a -> ModuleGraph a -> Bool
idAndTopSortFindsCycle a g = isGraphHMIdentity (a,g) && topSortFindsCycle g

idAndNotTopSortFindsCycle :: (Eq a, Hashable a) => a -> ModuleGraph a -> Bool
idAndNotTopSortFindsCycle a g = isGraphHMIdentity (a,g) && not (topSortFindsCycle g)

runTopSort :: (Eq a, Hashable a) => ModuleGraph a -> Either [a] [HS.HashSet a]
runTopSort g = fmap (HS.fromList . fmap fst) <$>
  topSort (HM.fromList . fmap (join (,)) . HM.keys $ g) (g' HM.!) 
  where g' = fmap HS.toList g

prop_isGraphHMIdentity_possibly_cyclic_int :: Property
prop_isGraphHMIdentity_possibly_cyclic_int = forAll (possiblyCyclic smallInt) isGraphHMIdentity

prop_isGraphHMIdentity_possibly_cyclic_str :: Property
prop_isGraphHMIdentity_possibly_cyclic_str = forAll (possiblyCyclic arbModuleName) isGraphHMIdentity

prop_isGraphHMIdentity_impossibly_cyclic :: Property
prop_isGraphHMIdentity_impossibly_cyclic = forAll acyclic isGraphHMIdentity

prop_isGraphHMIdentity_definitely_cyclic_int :: Property
prop_isGraphHMIdentity_definitely_cyclic_int = forAll (cyclic smallInt) isGraphHMIdentity

prop_isGraphHMIdentity_definitely_cyclic_str :: Property
prop_isGraphHMIdentity_definitely_cyclic_str = forAll (cyclic arbModuleName) isGraphHMIdentity

-- Test that fetchGraphM is fine with circular deps, 
-- but then topSort detects and fails on a circle for the same data
prop_topSort_detects_cycles_int :: Property
prop_topSort_detects_cycles_int = forAll (cyclic smallInt) (uncurry idAndTopSortFindsCycle)
prop_topSort_detects_cycles_str :: Property
prop_topSort_detects_cycles_str = forAll (cyclic arbModuleName) (uncurry idAndTopSortFindsCycle)

prop_topSort_detects_no_cycles :: Property
prop_topSort_detects_no_cycles = forAll acyclic (uncurry idAndNotTopSortFindsCycle)

-- Invariant: Every element of the result is non-empty.
prop_topSort_elements_nonEmpty :: Property
prop_topSort_elements_nonEmpty = forAll 
  ((possiblyCyclic smallInt) `suchThat` (\(_,g) -> not $ topSortFindsCycle g)) 
  (\(i, g) -> either (const False) (all (not . HS.null)) (runTopSort g))

-- topSort never places values before or beside their dependencies
prop_topSort_correctly_orders_result :: Property
prop_topSort_correctly_orders_result = forAll 
  ((possiblyCyclic smallInt) `suchThat` (\(_,g) -> not $ topSortFindsCycle g)) 
  (\(i, g) -> either (const False) (f . reverse) (runTopSort g)) where
    f = fst . foldl h (True, HS.empty) where
      h (b, acc) next = (b && HS.null (next `HS.intersection` acc), acc `HS.union` next)

-- fetchGraphM always fails for non-empty search starts where the retrieval function always fails
prop_fetchGraphM_Fails_Properly :: Property
prop_fetchGraphM_Fails_Properly = forAll 
  ((possiblyCyclic smallInt) `suchThat` (\(_,g) -> HM.size g > 1))
  (\(root, g) -> Left "no" ==
    fetchGraphM ((fmap HS.toList g) HM.!) (\_ _ -> Left "no") (join HM.singleton root))

prop_fetchGraphM_nodeps_is_identity =
  let fgm = runIdentity . fetchGraphM (const []) undefined in
  \g -> fgm g == (g :: HM.HashMap Int Int)

prop_fetchGraphM_takes_one_step =
  \i g g' -> runIdentity (fetchGraphM (const (HM.keys g'))
                                      (const . return . (g' HM.!))
                                      (HM.insert i i g))
           == (HM.insert i i g `HM.union` g' :: HM.HashMap Int Int)

tests = $testGroupGenerator
