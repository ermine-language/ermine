{-# OPTIONS_GHC -fno-warn-type-defaults #-}
--------------------------------------------------------------------
-- |
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
  , prePunctuate
  , prePunctuate'
  , block
  , say
  , sayLn
  , chooseNames
  ) where

import Control.Applicative
import Control.Monad.IO.Class
import Control.Lens
import Data.Bifunctor
import Data.Maybe
import Data.Semigroup
import Data.Text (unpack)
import Ermine.Syntax.Hint
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
      (pr,sf) = (bimap (fmap fst) (fmap fst)) $ span (\ (_,d) -> k + d < n) $ zip xs ls
      ls = tail $ scanl (\a b -> a + length b) 0 xs
      xs = hyphenate english_US t
  in if null pr
     then text (concat sf)
     else if null sf
          then text (concat pr)
          else vsep [text (concat pr) <> char '-', text (concat sf)]

prePunctuate :: Doc -> [Doc] -> [Doc]
prePunctuate _ [    ] = []
prePunctuate p (d:ds) = d : map (p <+>) ds

prePunctuate' :: Doc -> Doc -> [Doc] -> [Doc]
prePunctuate' _  _ [    ] = []
prePunctuate' fp p (d:ds) = (fp <+> d) : map (p <+>) ds

block :: [Doc] -> Doc
block [    ] = text "{}"
block (d:ds) = sep (lbrace <+> d : map (semi <+>) ds) <> line <> rbrace

say :: MonadIO m => Doc -> m ()
say = liftIO . displayIO stdout . renderPretty 0.8 80

sayLn :: MonadIO m => Doc -> m ()
sayLn d = say (d <> linebreak)

chooseNames :: (String -> Bool) -> [Hinted v] -> [String] -> ([String], [String])
chooseNames p ahs = go p ahs . filter (\n -> n `notElem` avoid && not (p n))
 where
 avoid = [ unpack h | Hinted h _ <- ahs ]

 go _     [] supply = ([], supply)
 go taken (Unhinted _ : hs) (n:supply) = (n:) `first` go taken hs supply
 go taken (Hinted h _ : hs) supply@(n:ns)
   | taken h'  = (n:) `first` go taken hs ns
   | otherwise = (h':) `first` go (\x -> x == h' || taken x) hs supply
  where h' = unpack h
 go _ _ _ = error "PANIC: chooseNames: ran out of names"
