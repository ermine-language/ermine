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
import Data.Word
import Ermine.Pretty
import Ermine.Syntax.Convention
import Ermine.Syntax.G

prettyG :: [Doc] -> Conventional Word32 -> (Ref -> Doc) -> G -> Doc
prettyG vs i pr (Case e bs) =
      "case"
  <+> prettyG vs i pr e
  <+> "of"
  <+> prettyContinuation vs i pr bs
prettyG _  _ pr (App f xs) = prettyFunc pr f <+> semiBraces (pr <$> xs)
prettyG vs i pr (Let bs e) = prettyLet vs i pr False bs e
prettyG vs i pr (LetRec bs e) = prettyLet vs i pr True bs e
prettyG _  _ _  (Lit w) = text $ show w

prettyFunc :: (Ref -> Doc) -> Func -> Doc
prettyFunc pr (Ref r) = pr r
prettyFunc _  (Con t) = "<" <> text (show t) <> ">"

prettyContinuation :: [Doc] -> Conventional Word32 -> (Ref -> Doc) -> Continuation -> Doc
prettyContinuation vs i pr (Cont bs d) = block $ (f <$> Map.toList bs) ++ ldd
 where
 ldd = maybeToList $ (\c -> "_ ->" <+> prettyG vs i pr c) <$> d
 f (t, (n, c)) = let (ws, rest) = splitConventional n vs
                     pr' (Local cc m) | ic <- index i cc, m >= ic = index ws cc !! fromIntegral (m - ic)
                     pr' r = pr r
                  in "<"
                  <> text (show t)
                  <> ">"
                 <+> prettyConventional ws ("->" <+> prettyG rest (i+fmap fromIntegral n) pr' c)

prettyConventional :: Conventional [Doc] -> Doc -> Doc
prettyConventional cvs d = hsep $ ifoldMap go cvs ++ [d] where
  go cc ws = text (show cc) <> semiBraces ws <$ guard (not (Prelude.null ws))

splitConventional :: Integral i => Conventional i -> [a] -> (Conventional [a], [a])
splitConventional = runState . traverse (state . splitAt . fromIntegral)

partitionRefs :: Foldable f => f Ref -> Conventional [Ref]
partitionRefs = Foldable.foldr go (return []) where
  go r = conv (r^.refConvention) %~ (r:)

prettyLet :: [Doc] -> Conventional Word32 -> (Ref -> Doc) -> Bool -> [PreClosure] -> G -> Doc
prettyLet vs i pr rec bs e =
      "let"
  <+> block (zipWith (\w bd -> w <+> "=" <+> bd) ws bds)
  <+> "in"
  <+> prettyG rest (i & conv C +~ fromIntegral bl) pr' e
 where
 bl = length bs
 (ws, rest) = splitAt bl vs
 bds = prettyPreClosure rest (if rec then pr' else pr) <$> bs
 pr' (Local C n) | ic <- i^.conv C, n >= ic = ws !! fromIntegral (n-ic)
 pr' r = pr r

prettyPreClosure :: [Doc] -> (Ref -> Doc) -> PreClosure -> Doc
prettyPreClosure vs pr (fvs, LambdaForm fa ba up bo) =
    dfvs <+> dl <+> prettyConventional cbvs ("->" <+> prettyG rest (fa + fmap fromIntegral ba) pr' bo)
 where
 dl = "\\" <> if up then "u" else "n"
 dfvs = semiBraces $ pr <$> fvs
 (cbvs, rest) = splitConventional ba vs
 cfvs = partitionRefs fvs
 pr' (Local c n)
   | n < fac  = pr $ index cfvs c !! fromIntegral n
   | n < fac + bac = index cbvs c !! fromIntegral (n-fac)
   | otherwise = error "PANIC: prettyPreClosure: Bad variable reference"
   where fac = index fa c
         bac = fromIntegral $ index ba c
 pr' r = pr r
