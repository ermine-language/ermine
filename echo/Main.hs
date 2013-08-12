{-# LANGUAGE TemplateHaskell #-}
module Main ( main ) where

import Control.Exception (catch, SomeException)
import qualified Data.Binary as Binary
import qualified Data.ByteString.Lazy as B
import Data.List (intersperse)
import Data.Monoid (mappend, mconcat)
import Data.Text as SText hiding (intersperse, map)
import Ermine.Syntax.Core
import Ermine.Syntax.Literal as Lit

main :: IO ()
main = do
       input <- B.readFile "core.in"
       putStrLn . showbytes $ input
       --putStrLn . showbytes . writeCore $ (Lit . Lit.Char $ 'A')
       --putStrLn . showbytes . writeCore $ (Lit . Lit.String $ SText.pack "A")
       let r = B.writeFile "core.out" . writeCore $ readCore input
       catch r (writeFile "failure.txt" . mappend "failed: " . (show :: SomeException -> String))

readCore :: B.ByteString -> HardCore
readCore = Binary.decode

showbytes :: B.ByteString -> String
showbytes = mconcat . ("Bytes: ":) . intersperse ", " . map show . B.unpack

writeCore :: HardCore -> B.ByteString
writeCore = Binary.encode

{-
main :: IO ()
main = do
       input <- B.getContents
       let l = B.length input
       l `seq` return ()
       writeFile "echoBytesIn.txt" $ mconcat ["Read ", show l, " bytes\n"]
       let out = writeCore $ readCore input
       writeFile "echoBytesOut.txt" $ mconcat ["Read ", show l, " bytes, wrote ", show (B.length out), " bytes.\n"]
       B.putStr out
-}