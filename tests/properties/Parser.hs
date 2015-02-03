{-# LANGUAGE TemplateHaskell #-}
module Parser (tests) where

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

type ModuleName = String
newtype ModuleGraph = 
	ModuleGraph (HM.HashMap ModuleName (HS.HashSet ModuleName))
  deriving Show

unionModuleGraphs :: ModuleGraph -> ModuleGraph -> ModuleGraph
unionModuleGraphs (ModuleGraph g1) (ModuleGraph g2) = 
  ModuleGraph (HM.unionWith HS.union g1 g2)

nameToModuleGraph :: ModuleName -> ModuleGraph
nameToModuleGraph m = ModuleGraph $ HM.singleton m HS.empty

singletonModuleGraph :: ModuleName -> [ModuleName] -> ModuleGraph
singletonModuleGraph m deps = ModuleGraph $ HM.singleton m (HS.fromList deps)

addNode :: ModuleName -> [ModuleName] -> ModuleGraph -> ModuleGraph
addNode m ms g = unionModuleGraphs (singletonModuleGraph m ms) g

addLevel :: [(ModuleName, [ModuleName])] -> ModuleGraph -> ModuleGraph
addLevel ms g = foldl (flip $ uncurry addNode) g ms

instance Arbitrary ModuleGraph where
  arbitrary = sized f where
    f 0 = nameToModuleGraph <$> arbModuleName
    f n = smaller $ arbitrary >>= arbLevel

arbLevel :: ModuleGraph -> Gen ModuleGraph
arbLevel mg@(ModuleGraph g) = 
  let ks  = HM.keys g in do
    ms    <- Nel.toList <$> genNel ((,) <$> arbModuleName <*> arbitrary)
    let lvl = fmap (second $ flip take ks) ms
    return $ addLevel lvl mg

arbModuleName :: Gen ModuleName
arbModuleName = nelToMName <$> genNel modPart where
  nelToMName  = concat . Nel.toList . Nel.intersperse "."
  modPart     = fmap (take 10) $ (:) <$> upperAlpha <*> listOf lowerAlpha
  letters     = "abcdefghijklmnopqrstuvwxyz"
  lowerAlpha  = oneof (map return letters)
  upperAlpha  = oneof (map (return . toUpper) letters)

prop_fetchGraphHMIdentity :: ModuleGraph -> Bool
prop_fetchGraphHMIdentity = f . addMasterNode where
  f (ModuleGraph g) = trace (show g) (a == b) where
    a = HS.fromList (HM.keys g)
    b = HS.fromList . HM.keys . runIdentity $
    	  fetchGraphM ((fmap HS.toList g) HM.!) (const . return)
        	(join HM.singleton "Control.Monad")
  addMasterNode :: ModuleGraph -> ModuleGraph
  addMasterNode mg@(ModuleGraph g) = addNode "Control.Monad" (HM.keys g) mg

prop_fetchGraphM_nodeps_is_identity =
  let fgm = runIdentity . fetchGraphM (const []) undefined in
  \g -> fgm g == (g :: HM.HashMap Int Int)

prop_fetchGraphM_takes_one_step =
  \i g g' -> runIdentity (fetchGraphM (const (HM.keys g'))
                                      (const . return . (g' HM.!))
                                      (HM.insert i i g))
           == (HM.insert i i g `HM.union` g' :: HM.HashMap Int Int)

tests = $testGroupGenerator
