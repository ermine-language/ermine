{-# LANGUAGE OverloadedStrings #-}
module Ermine.Pretty.G
  ( prettyG
  ) where

import Control.Lens hiding (index)
import Control.Monad.State
import Data.Foldable as Foldable
import Data.Functor
import Data.Functor.Rep
import Data.Maybe
import Data.Map as Map
import Data.Monoid
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Word
import Ermine.Pretty
import Ermine.Syntax.Sort
import Ermine.Syntax.G

prettyG :: [Doc] -> Sorted Word32 -> (Sort -> Ref -> Doc) -> G -> Doc
prettyG vs i pr (Case e bs) =
      "case"
  <+> prettyG vs i pr e
  <+> "of"
  <+> prettyContinuation vs i pr bs
prettyG vs i pr (CaseLit r bs) =
      "case#"
  <+> pr U r
  <+> "of"
  <+> prettyContinuation vs i pr bs
prettyG _  _ pr (App f xs)    = hsep $ prettyFunc pr f : prettySorted (imap (fmap Foldable.toList . fmap . pr) xs)
prettyG vs i pr (Let bs e)    = prettyLet vs i pr False bs e
prettyG vs i pr (LetRec bs e) = prettyLet vs i pr True bs e
prettyG _  _ _  (Lit w)       = text $ show w

prettyFunc :: (Sort -> Ref -> Doc) -> Func -> Doc
prettyFunc pr (Ref r) = pr B r
prettyFunc _  (Con t) = "<" <> text (show t) <> ">"

prettyContinuation :: [Doc] -> Sorted Word32 -> (Sort -> Ref -> Doc) -> Continuation -> Doc
prettyContinuation vs i pr (Continuation bs d) = block $ (f <$> Map.toList bs) ++ ldd
 where
 ldd = maybeToList $ (\c -> "_ ->" <+> prettyG vs i pr c) <$> d
 f (t, (n, c)) = let (ws, rest) = splitSorted n vs
                     pr' cc (Local m) | ic <- index i cc, m >= ic = index ws cc !! fromIntegral (m - ic)
                     pr' cc r = pr cc r
                  in "<"
                  <> text (show t)
                  <> ">"
                 <+> hsep (prettySorted ws ++ ["->" <+> prettyG rest (i+fmap fromIntegral n) pr' c])

prettySorted :: Sorted [Doc] -> [Doc]
prettySorted cvs = ifoldMap go cvs where
  go cc ws = text (show cc) <> semiBraces ws <$ guard (not (Prelude.null ws))

splitSorted :: Integral i => Sorted i -> [a] -> (Sorted [a], [a])
splitSorted = runState . traverse (state . splitAt . fromIntegral)

prettyLet :: [Doc] -> Sorted Word32 -> (Sort -> Ref -> Doc) -> Bool -> Vector PreClosure -> G -> Doc
prettyLet vs i pr rec bs e =
      "let"
  <+> block (zipWith (\w bd -> w <+> "=" <+> bd) ws (Foldable.toList bds))
  <+> "in"
  <+> prettyG rest (i & sort B +~ fromIntegral bl) pr' e
 where
 bl = V.length bs
 (ws, rest) = splitAt bl vs
 bds = prettyPreClosure rest (if rec then pr' else pr) <$> bs
 pr' B (Local n) | ic <- index i B, n >= ic = ws !! fromIntegral (n-ic)
 pr' c r = pr c r

prettyPreClosure :: [Doc] -> (Sort -> Ref -> Doc) -> PreClosure -> Doc
prettyPreClosure vs pr (PreClosure cfvs (LambdaForm fa ba up bo)) = hsep $
   dfvs : dl : prettySorted cbvs ++ ["->" <+> prettyG rest (fa + fmap fromIntegral ba) pr' bo]
 where
 dl = "\\" <> if up then "u" else "n"
 dfvs = semiBraces $ join $ Foldable.toList $ imap (fmap Foldable.toList . fmap . pr) cfvs
 (cbvs, rest) = splitSorted ba vs
 pr' c (Local n)
   | n < fac = pr c $ index cfvs c V.! fromIntegral n
   | n < fac + bac  = index cbvs c !! fromIntegral (n-fac)
   | otherwise = error "PANIC: prettyPreClosure: Bad variable reference"
   where fac = index fa c
         bac = fromIntegral $ index ba c
 pr' c r = pr c r
