{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) McGraw Hill Financial 2014
-- License   :  BSD2
-- Maintainer:  Stephen Compall <scompall@nocandysw.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------

module Ermine.Loader.Filesystem
  ( filesystemLoader
  , LoadRefusal(..)
  , explainLoadRefusal
  , Freshness()
  ) where

import Control.Monad.Trans
import Control.Monad.Trans.Except
import Data.Data
import Data.Monoid
import Data.Text
import Ermine.Loader.Core
import GHC.Generics
import qualified System.FilePath as P

-- | A 'Loader' that searches an area of the filesystem for modules
-- matching the given module name, and results in the textual contents
-- of that module file.
--
-- Non-IO errors are reported as 'LoadRefusal'.  Use
-- 'Ermine.Loader.Core.loaded' and 'withExceptT' to translate this to
-- a monoidal type (e.g. '[]') for combination with other loaders.
filesystemLoader :: MonadIO m =>
                    P.FilePath     -- ^ Filesystem root to start the search.
                    -> String      -- ^ File extension.
                    -> Loader Freshness (ExceptT LoadRefusal m) Text Text
filesystemLoader root ext =
  Loader (\n -> undefined) (\n cv -> undefined)

-- | A recoverable load error that shouldn't prevent alternative
-- loaders from being tried for a given module.
data LoadRefusal =
    FileNotFound P.FilePath -- ^ No file with that name was found.
  | NoFilenameMapping       -- ^ There's no mapping from the module
                            -- name to a valid filesystem name.
  deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)

explainLoadRefusal :: LoadRefusal -> Text
explainLoadRefusal (FileNotFound fp) =
  "There was no file named " <> (pack fp)
explainLoadRefusal NoFilenameMapping =
  "The module's name has no valid associated filename"

newtype Freshness = Freshness Int
  deriving (Show, Read, Data, Typeable, Generic)
