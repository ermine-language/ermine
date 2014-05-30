{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett 2014
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable (DeriveDataTypeable)
--
--------------------------------------------------------------------
module Ermine.Builtin.Global
  (
  -- * lists
    nilg
  , consg
  -- * maybe
  , nothingg
  , justg
  -- * tuples
  , tupleg
  -- * booleans
  , trueg
  , falseg
  -- * existential test constructor
  , eg
  -- * numeric literals
  , literalg
  -- * string literals
  , stringg
  -- * primitive types
  , stringhg
  , inthg
  , longhg
  , integerhg
  -- * primops
  , putStrLng
  , showIntg
  , showLongg
  , addLongg
  , fromIntegerToIntg
  , fromIntegerToLongg
  ) where

import Data.Text
import Data.Word
import Ermine.Syntax.Global
import Ermine.Syntax.ModuleName

builtin :: Fixity -> Text -> Global
builtin f = glob f (mkModuleName_ "Prelude")

builtin_ :: Text -> Global
builtin_ = builtin Idfix

nilg, consg, nothingg, justg, eg :: Global
nilg     = builtin_ "[]"
consg    = builtin (Infix R 5) "(::)"
nothingg = builtin (Infix R 5) "Nothing"
justg    = builtin_ "Just"
eg       = builtin_ "E"

tupleg :: Word64 -> Global
tupleg w8 = builtin_ $ pack $ "(" ++ Prelude.replicate (if n == 0 then 0 else n-1) ',' ++ ")"
  where n = fromIntegral w8

trueg, falseg :: Global
trueg  = builtin_ "True"
falseg = builtin_ "False"

literalg :: Global
literalg = builtin_ "Literal"

stringg :: Global
stringg = builtin_ "String"

stringhg :: Global
stringhg = builtin_ "String#"

inthg :: Global
inthg = builtin_ "Int#"

longhg :: Global
longhg = builtin_ "Long#"

integerhg :: Global
integerhg = builtin_ "Integer#"

putStrLng :: Global
putStrLng = builtin_ "putStrLn"

showIntg :: Global
showIntg = builtin_ "showInt"

showLongg :: Global
showLongg = builtin_ "showLong"

addLongg :: Global
addLongg = builtin_ "addLongg"

fromIntegerToIntg :: Global
fromIntegerToIntg = builtin_ "fromIntegerToInt"

fromIntegerToLongg :: Global
fromIntegerToLongg = builtin_ "fromIntegerToLong"
