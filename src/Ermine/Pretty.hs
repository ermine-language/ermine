{-# OPTIONS_GHC -fno-warn-type-defaults #-}
--------------------------------------------------------------------
-- |
-- Module    :  Ermine.Pretty
-- Copyright :  (c) Edward Kmett and Dan Doel 2012-2013
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Ermine.Pretty
  ( module Text.PrettyPrint.ANSI.Leijen
  , names
  , parensIf
  , hyph
  , say
  , sayLn
  ) where

import Control.Applicative
import Control.Arrow
import Control.Monad.IO.Class
import Control.Lens
import Data.Maybe
import Data.Semigroup
import Numeric.Lens
import System.IO
import Text.Hyphenation
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>), (<>))
import Text.Trifecta.Delta () -- for Text.Trifecta.Instances

-- | This is an infinitely large free variable supply you can trim your used variables out of.
names :: [String]
names = map pure az
    ++ [ i : review (base 36) j | j <- [1..], i <- az ] where
  az = ['a'..'z']

-- | Pretty print parentheses
parensIf :: Bool -> Doc -> Doc
parensIf True  = parens
parensIf False = id

-- | Hyphenate a word using standard TeX-style english_US hyphenation.
hyph :: String -> Doc
hyph t = column $ \k -> columns $ \mn ->
  let n = fromMaybe 80 mn
      (pr,sf) = (fmap fst *** fmap fst) $ span (\ (_,d) -> k + d < n) $ zip xs ls
      ls = tail $ scanl (\a b -> a + length b) 0 xs
      xs = hyphenate english_US t
  in if null pr
     then text (concat sf)
     else if null sf
          then text (concat pr)
          else vsep [text (concat pr) <> char '-', text (concat sf)]

say :: MonadIO m => Doc -> m ()
say = liftIO . displayIO stdout . renderPretty 0.8 80

sayLn :: MonadIO m => Doc -> m ()
sayLn d = say (d <> linebreak)
