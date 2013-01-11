{-# LANGUAGE TemplateHaskell #-}
module Main
  ( main
  ) where

import Control.Lens
import Control.Monad.State.Strict
import Data.Char
import Data.HashSet as HashSet
import Data.List
import Data.Set as Set
import Data.Set.Lens
import Data.Monoid
import Ermine.Parser.Keywords
import Ermine.Version
import System.Console.Haskeline

startingKeywordSet, keywordSet :: Set String
startingKeywordSet = setOf folded startingKeywords
keywordSet         = setOf folded keywords

data ReplState = ReplState { _replIds :: Set String }

makeLenses ''ReplState

type Repl = InputT (StateT ReplState IO)

defaultReplState :: ReplState
defaultReplState = ReplState mempty

loading :: String -> Bool
loading zs = isPrefixOf ":l" xs && isPrefixOf xs ":load"
  where xs = takeWhile (not . isSpace) $ dropWhile isSpace zs

completed :: (String,String) -> StateT ReplState IO (String, [Completion])
completed (ls, rs)
  | ' ' `notElem` ls = completeWith startingKeywordSet (ls, rs)
  | loading rls = completeFilename (ls, rs)
  | otherwise   = do completeWith keywordSet (ls, rs)
  where rls = reverse ls

completeWith :: Set String -> CompletionFunc (StateT ReplState IO)
completeWith kws = completeWord Nothing " ,()[]{}" $ \s -> do
  strs <- use replIds
  return $ (strs <> kws)^..folded.filtered (s `isPrefixOf`).to (\o -> Completion o o True)

settings :: Settings (StateT ReplState IO)
settings = setComplete completed defaultSettings
  { historyFile = Just ".ermine_history"
  }

main :: IO ()
main = do
  flip evalStateT defaultReplState $ runInputT settings $ do
    outputStrLn logo
    loop

loop :: Repl ()
loop = do
  minput <- getInputLine ">> "
  case minput of
    Nothing     -> return ()
    Just "quit" -> return ()
    Just ""     -> loop
    Just input -> do
      outputStrLn $ "Input was: " ++ input
      loop
