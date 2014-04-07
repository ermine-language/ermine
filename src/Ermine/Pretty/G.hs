
module Ermine.Pretty.G
  ( prettyG
  ) where

import Data.Functor
import Data.Maybe
import Data.Map
import Data.Word
import Ermine.Pretty
import Ermine.Syntax.G

prettyG :: [String] -> Word32 -> (Ref -> Doc) -> G -> Doc
prettyG vs i pr (Case e bs) =
      text "case"
  <+> prettyG vs i pr e
  <+> text "of"
  <+> prettyContinuation vs i pr bs
prettyG _  _ pr (App f xs) = prettyFunc pr f <+> semiBraces (pr <$> xs)
prettyG vs i pr (Let bs e) = prettyLet vs i pr False bs e
prettyG vs i pr (LetRec bs e) = prettyLet vs i pr True bs e
prettyG _  _ _  (Lit w) = text $ show w

prettyFunc :: (Ref -> Doc) -> Func -> Doc
prettyFunc pr (Ref r) = pr r
prettyFunc _  (Con t) = text $ "<" ++ show t ++ ">"

prettyContinuation :: [String] -> Word32 -> (Ref -> Doc) -> Continuation -> Doc
prettyContinuation vs i pr (Cont bs d) = block $ (f <$> toList bs) ++ ldd
 where
 ldd = maybeToList $ (\c -> text "_ ->" <+> prettyG vs i pr c) <$> d
 f (t, (n, c)) = let (ws, rest) = splitAt (fromIntegral n) vs
                     pr' (Local m) | m >= i = text $ ws !! fromIntegral (m - i)
                     pr' r = pr r
                  in text ("<" ++ show t ++ ">")
                       <+> hsep (text <$> ws)
                       <+> text "->"
                       <+> prettyG rest (i+fromIntegral n) pr' c

prettyLet :: [String] -> Word32 -> (Ref -> Doc) -> Bool -> [PreClosure] -> G -> Doc
prettyLet vs i pr rec bs e =
      text "let"
  <+> block (zipWith (\w bd -> text (w ++ " =") <+> bd) ws bds)
  <+> text "in"
  <+> prettyG rest (i+fromIntegral bl) pr' e
 where
 bl = length bs
 (ws, rest) = splitAt bl vs
 bds = prettyPreClosure rest (if rec then pr' else pr) <$> bs
 pr' (Local n) | n >= i = text $ ws !! fromIntegral (n-i)
 pr' r = pr r

prettyPreClosure :: [String] -> (Ref -> Doc) -> PreClosure -> Doc
prettyPreClosure vs pr (fvs, LambdaForm fa ba up bo) =
      dfvs <+> dl <+> dws <+> text "->"
  <+> prettyG rest (fa + fromIntegral ba) pr' bo
 where
 dl = text $ "\\" ++ if up then "u" else "n"
 dfvs = semiBraces $ pr <$> fvs
 dws = semiBraces $ text <$> ws
 (ws, rest) = splitAt (fromIntegral ba) vs
 pr' (Local n) | n < fa = pr $ fvs !! fromIntegral n
               | n-fa < fromIntegral ba = text $ ws !! fromIntegral (n-fa)
               | otherwise = error "Bad variable reference"
 pr' r = pr r
