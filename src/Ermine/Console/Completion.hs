module Ermine.Console.Completion
  ( settings
  ) where

import Control.Lens
import Data.Char
import Data.List
import Data.Set as Set
import Data.Set.Lens
import Data.Monoid
import Ermine.Parser.Keywords
import Ermine.Console.Command
import Ermine.Console.State
import System.Console.Haskeline

startingKeywordSet, keywordSet :: Set String
startingKeywordSet = setOf folded startingKeywords
                  <> setOf (folded.name.to (':':)) commands
keywordSet         = setOf folded keywords

loading :: String -> Bool
loading zs = isPrefixOf ":l" xs && isPrefixOf xs ":load"
  where xs = takeWhile (not . isSpace) $ dropWhile isSpace zs

completed :: (String,String) -> Console (String, [Completion])
completed (ls, rs)
  | ' ' `notElem` ls = completeWith startingKeywordSet (ls, rs)
  | loading rls = completeFilename (ls, rs)
  | otherwise   = completeWith keywordSet (ls, rs)
  where rls = reverse ls

completeWith :: Set String -> CompletionFunc Console
completeWith kws = completeWord Nothing " ,()[]{}" $ \s -> do
  strs <- use consoleIds
  return $ (strs <> kws)^..folded.filtered (s `isPrefixOf`).to (\o -> Completion o o True)

settings :: Settings Console
settings = setComplete completed defaultSettings
  { historyFile = Just ".ermine_history"
  }
