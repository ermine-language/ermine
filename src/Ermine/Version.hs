--------------------------------------------------------------------
-- |
-- Module    :  Ermine.Version
-- Copyright :  (c) Edward Kmett and Dan Doel 2012
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
-- This module provides access to the Ermine logo and the version number.
--------------------------------------------------------------------
module Ermine.Version
  ( logo
  , version
  ) where

import Control.Applicative
import Control.Exception.Lens
import Data.List.Split
import Data.Version
import Paths_ermine
import System.FilePath
import System.Random
import System.IO.Unsafe

-- | Obtain a copy of the Ermine logo.
logo :: String
logo = unsafePerformIO $ randomRIO (0,29 :: Int) >>= \n -> if n == 0 then logos else rat

ver :: String
ver = showVersion version { versionTags = ["α"] }

copyright :: String
copyright = "© 2010-2013 S&P Capital IQ"

allrights :: String
allrights = "All Rights Reserved"

logos :: IO String
logos = handling_ id rat $ do
  file <- getDataFileName $ "data" </> "logos.txt"
  txt <- splitOn [""] . lines <$> readFile file
  nm:xs@(l1:l2:l3:l4:rest) <- (txt !!) <$> randomRIO (0, length txt - 1)
  let n = maximum (map length xs)
  let pad ys = ys ++ replicate (n - length ys) ' '
  return $ unlines $
    [ l1
    , pad l2 ++ ' ' : nm ++ ' ' : ver
    , pad l3 ++ ' ' : copyright
    , pad l4 ++ ' ' : allrights
    ] ++ rest

rat :: IO String
rat = handling_ id (return bad) $ getDataFileName ("data" </> "logo.txt") >>= readFile

bad :: String
bad = "Ermine " ++ ver ++ "\nwarning: Missing data/logo.txt"
