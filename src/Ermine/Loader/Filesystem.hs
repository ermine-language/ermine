{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
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

import Control.Applicative
import Control.Exception (tryJust)
import Control.Lens hiding ((<.>))
import Control.Monad.Error.Class
import Control.Monad.State
import Control.Monad.Trans.Except
import Data.Data
import Data.Function (on)
import Data.Monoid
import Data.Text (Text, pack)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text.Lens (text)
import Data.Time.Clock (UTCTime)
import Ermine.Loader.Core
import GHC.Generics
import System.Directory (getModificationTime)
import System.FilePath ((</>), (<.>))
import qualified System.FilePath as P
import System.IO.Error.Lens (errorType, _NoSuchThing)

-- | A 'Loader' that searches an area of the filesystem for modules
-- matching the given module name, and results in the textual contents
-- of that module file.
--
-- Non-IO errors are reported as 'LoadRefusal'.  Use
-- 'Ermine.Loader.Core.loaded' and 'withExceptT' to translate this to
-- a monoidal type (e.g. '[]') for combination with other loaders.
filesystemLoader :: (MonadIO m, MonadError LoadRefusal m) =>
                    P.FilePath     -- ^ Filesystem root to start the search.
                    -> String      -- ^ File extension.
                    -> Loader Freshness m Text Text
filesystemLoader root ext =
  testBasedCacheLoader pathName
      (\pn -> trapNoSuchThing pn . TIO.readFile $ pn)
      (\pn -> liftM Freshness . trapNoSuchThing pn
              . getModificationTime $ pn)
      ((/=) `on` modTime)
  where pathName = hoistEither . runExcept
                   . fmap (\n -> root </> n <.> ext)
                   . moduleFileName
        trapNoSuchThing pn = hoistEither <=< liftIO
                           . (lifted._Left .~ FileNotFound pn)
                           . tryJust (^? errorType._NoSuchThing)

hoistEither :: MonadError e m => Either e a -> m a
hoistEither = either throwError return

-- | A recoverable load error that shouldn't prevent alternative
-- loaders from being tried for a given module.
data LoadRefusal =
    FileNotFound P.FilePath -- ^ No file with that name was found.
  | NoFilenameMapping       -- ^ There's no mapping from the module
                            -- name to a valid filesystem name.
  deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)

explainLoadRefusal :: LoadRefusal -> Text
explainLoadRefusal (FileNotFound fp) =
  "There was no file named " <> pack fp
explainLoadRefusal NoFilenameMapping =
  "The module's name has no valid associated filename"

newtype Freshness = Freshness {modTime :: UTCTime}
  deriving (Show, Read, Data, Typeable, Generic)

-- | Convert a module name to a relative filename, minus the
-- extension, or fail for some reason.
moduleFileName :: Text -> Except LoadRefusal P.FilePath
moduleFileName modName = do
  let modName' = modName ^.. text
  null (invalidChars modName) `unless` throwE NoFilenameMapping
  let relPath = set (mapped.filtered ('.'==)) P.pathSeparator modName'
  P.isValid relPath `unless` throwE NoFilenameMapping
  return relPath

newtype ApMonoid f a = ApMonoid {runApMonoid :: f a}
instance (Applicative f, Semigroup a) => Semigroup (ApMonoid f a) where
  ApMonoid l <> ApMonoid r = ApMonoid $ liftA2 (<>) l r
instance (Applicative f, Monoid a) => Monoid (ApMonoid f a) where
  mempty = ApMonoid $ pure mempty
  ApMonoid l `mappend` ApMonoid r = ApMonoid $ liftA2 mappend l r

-- | Answer the positions of filesystem-invalid characters in the
-- given module name, and explain the problem with each.
invalidChars :: Text -> [(Int, Text)]
invalidChars t = [(-1, "Can't be empty") | T.null t ]
                 ++ [(0, "Can't start with a dot")
                    | anyOf folded (=='.') (firstOf text t)]
                 ++ checkChars t
                 ++ [(T.length t - 1, "Can't end with a dot")
                    | anyOf folded (=='.') (lastOf text t)]
  where checkChars = flip evalState False . runApMonoid
                     . ifoldMapOf text (fmap ApMonoid . lookAtChar)
        lookAtChar :: Int -> Char -> State Bool [(Int, Text)]
        lookAtChar i ch = do
          let isDot = ch == '.'
          lastDot <- get
          put isDot
          return . map (i,) $
            ["Empty module path components aren't allowed" | lastDot && isDot]
            ++ ["Disallowed character: " <> pack [ch] | P.isPathSeparator ch]
            ++ ["Null characters aren't allowed" | '\NUL' == ch]
