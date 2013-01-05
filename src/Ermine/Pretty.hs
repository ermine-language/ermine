{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Ermine.Pretty
  ( module System.Console.Terminfo.PrettyPrint
  , module Text.PrettyPrint.Free
  , names
  , parensIf
  , hyph
--  , displayed
  ) where

import Control.Applicative
import Control.Arrow
import Control.Lens
-- import Control.Monad.IO.Class
import Numeric.Lens
import System.Console.Terminfo.PrettyPrint
-- import System.Console.Haskeline
import Text.Hyphenation
import Text.PrettyPrint.Free
-- import Text.PrettyPrint.Free.Internal

names :: [String]
names = map pure az
    ++ [ i : review (base 36) j | j <- [1..], i <- az ] where
  az = ['a'..'z']

parensIf :: Bool -> Doc a -> Doc a
parensIf True  = parens
parensIf False = id

hyph :: String -> Doc a
hyph t = column $ \k -> columns $ \n ->
  let (pr,sf) = (fmap fst *** fmap fst) $ span (\ (_,d) -> k + d < n) $ zip xs ls
      ls = tail $ scanl (\a b -> a + length b) 0 xs
      xs = hyphenate english_US t
  in if pr == []
     then text (concat sf)
     else if sf == []
          then text (concat pr)
          else above (text (concat pr) <> char '-') (text (concat sf))

{-
displayed :: TermDoc -> IO ()
displayed = displayDoc 0.99

displays :: Doc e -> ShowS
displays = displayS . renderPretty 1.00 80

displayOut :: MonadIO m => Doc e -> InputT m ()
displayOut d = outputStrLn (displays d "")
-}
