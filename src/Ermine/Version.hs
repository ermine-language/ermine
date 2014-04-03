{-# LANGUAGE TemplateHaskell #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett and Dan Doel 2012-2014
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
-- This module provides access to the Ermine 'logo' and the current
-- 'version' number.
--------------------------------------------------------------------
module Ermine.Version
  ( logo
  , version
  ) where

import Control.Applicative
import Data.ByteString.UTF8 (toString)
import Data.FileEmbed
import Data.List.Split
import Data.Version
import qualified Paths_ermine
import System.FilePath
import System.Random
import System.IO.Unsafe

-- | Obtain a copy of the Ermine logo.
logo :: String
logo = unsafePerformIO $ randomRIO (0,29 :: Int) >>= \n -> if n == 0 then logos else return rat

-- | Grab the version number from this project.
version :: String
version = showVersion Paths_ermine.version { versionTags = ["α"] }

logos :: IO String
logos = do
  let txt = filter ((>3).length)
          . splitOn [""] . lines . toString $ $(embedFile $ "etc" </> "logos.txt")
  nm:xs@(l1:l2:l3:l4:rest) <- (txt !!) <$> randomRIO (0, length txt - 1)
  let n = maximum (map length xs)
  let pad ys = ys ++ replicate (n - length ys) ' '
  return $ unlines $
    [ l1
    , pad l2 ++ ' ' : nm ++ ' ' : version
    , pad l3
    , pad l4
    ] ++ rest

rat :: String
rat = toString $ $(embedFile $ "etc" </> "logo.txt")
