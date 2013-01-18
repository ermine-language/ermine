{-# LANGUAGE CPP, ForeignFunctionInterface #-}
module Ermine.Console.Unicode
  ( withUnicode
  ) where

import Control.Monad.IO.Class
import Control.Monad.CatchIO

##if defined(i386_HOST_ARCH)
##define USE_CP
import Control.Applicative
import Foreign.C.Types
foreign import stdcall "windows.h SetConsoleCP" c_SetConsoleCP :: CUInt -> IO Bool
foreign import stdcall "windows.h GetConsoleCP" c_GetConsoleCP :: IO CUInt
##elif defined(x64_64_HOST_ARCH)
##define USE_CP
import Control.Applicative
import Foreign.C.Types
foreign import ccall "windows.h SetConsoleCP" c_SetConsoleCP :: CUInt -> IO Bool
foreign import ccall "windows.h GetConsoleCP" c_GetConsoleCP :: IO CUInt
##endif

-- | Run in a modified codepage where we can print UTF-8 values on Windows.
--
-- You should probably run the top level of your program in this.
withUnicode :: MonadCatchIO m => m a -> m a
##ifdef USE_CP
withUnicode m = do
  cp <- c_GetConsoleCP
  (c_SetConsoleCP 65001 *> m) `finally` c_SetConsoleCP cp
##else
withUnicode m = m
##endif
