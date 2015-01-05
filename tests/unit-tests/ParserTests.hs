module ParserTests
  ( ermineParserTests
  , stdLibDir
  , main
  ) where

import Control.Applicative
import Control.Monad
import Data.Functor.Identity
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.Traversable
import Ermine.Parser.Module
import System.FilePath ((</>))
import Test.Framework.Providers.API
import Test.Framework.Providers.HUnit
import Test.HUnit

import TestDef

stdLibDir = "stdlib" </> "Prelude"

someGraph :: HM.HashMap String [String]
someGraph = HM.fromList
  [("Prelude", [])
  ,("Data.Functor", ["Prelude"])
  ,("Control.Apply", ["Prelude"])
  ,("Control.Applicative", ["Prelude", "Data.Functor", "Control.Apply"])
  ,("Control.Bind", ["Control.Apply", "Prelude"])
  ,("Control.Monad", ["Control.Applicative", "Control.Bind", "Prelude"])
  ]

-- How someGraph should load.
canonicalSGLoad :: [HS.HashSet String]
canonicalSGLoad = HS.fromList <$>
  [["Prelude"]
  ,["Data.Functor", "Control.Apply"]
  ,["Control.Applicative", "Control.Bind"]
  ,["Control.Monad"]]

fetchGraphHMIdentity =
  assertEqual "fetchGraph traverses a dependency map"
              (HS.fromList . HM.keys $ someGraph)
    . HS.fromList . HM.keys . runIdentity $
    fetchGraphM (someGraph HM.!) (const . return)
                (join HM.singleton "Control.Monad")

topSortSomeGraph =
  assertEqual "found canonical sort" (Right canonicalSGLoad) $
    fmap (HS.fromList . fmap fst) <$>
      topSort (HM.fromList . fmap (join (,)) . HM.keys $ someGraph)
              (someGraph HM.!)

ermineParserTests  = TestDef {
   name            = "Ermine Parser Tests"
 , dir             = stdLibDir
 , inputFileFormat = "*.e"
 , run             = \ermineFile -> do 
     ermineCode <- readFile ermineFile
     putStrLn ermineFile
     --TODO: put a real assertion here
     --parse ermineCode @?= someExpectedValueHere
}

main = TestGroup "ParserTests"
  [ testCase "fetchGraph is a hashmap identity" fetchGraphHMIdentity
  , testCase "topSort sorts someGraph canonically" topSortSomeGraph
  ]
