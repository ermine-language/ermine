{-# LANGUAGE OverloadedStrings #-}
module LoaderTests (main) where

import Control.Lens
import Control.Monad.Trans.Except
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Ermine.Loader.Core
import Ermine.Loader.Filesystem
import System.FilePath ((</>))
import Test.Framework.Providers.API
import Test.Framework.Providers.HUnit
import Test.HUnit

import ParserTests (stdLibDir)

stdLibLoader :: Loader Freshness (ExceptT LoadRefusal IO) Text Text
stdLibLoader = filesystemLoader stdLibDir "e"

loadsAFile = do fraw <- TIO.readFile (stdLibDir </> "Byte.e")
                lr <- (stdLibLoader ^. load) "Byte" & runExceptT
                assertEqual "load result" (Right fraw) (fmap snd lr)

main = TestGroup "LoaderTests"
  [ testCase "loads a file" loadsAFile
  ]
