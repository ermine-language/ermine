{-# LANGUAGE TemplateHaskell #-}
module Main ( main ) where

import qualified Data.Binary as Binary
import qualified Data.ByteString.Lazy as B
import Ermine.Syntax.Core

main :: IO ()
main = do
       input <- B.readFile "core.in"
       let r = writeCore $ readCore input
       B.writeFile "core.out" r

readCore :: B.ByteString -> Core Int
readCore = Binary.decode

writeCore :: Core Int -> B.ByteString
writeCore = Binary.encode