{-# LANGUAGE OverloadedStrings #-}
module Ermine.Pretty.G
  ( prettyG
  , defaultCxt
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
import Ermine.Pretty.Id
import Ermine.Syntax.Sort
import Ermine.Syntax.G

defaultCxt :: Sort -> Ref -> Doc
defaultCxt U (Lit n) = int (fromIntegral n)
defaultCxt s (Lit n) = "bad lit?" <+> text (show s) <> "{" <> int (fromIntegral n) <> "}"
defaultCxt s (Global n) = "global?" <+> text (show s) <> "{" <> prettyId n <> "}"
defaultCxt s (Stack n) = "stack?" <+> text (show s) <> "{" <> int (fromIntegral n) <> "}"
defaultCxt s (Local n) = "local?" <+> text (show s) <> "{" <> int (fromIntegral n) <> "}"

prettyG :: [Doc] -> (Sort -> Ref -> Doc) -> G -> Doc
prettyG vs pr (Case e bs) =
      "case"
  <+> prettyG vs pr e
  <+> "of"
  <+> prettyContinuation vs pr bs
prettyG vs pr (CaseLit r bs) =
      "case#"
  <+> pr U r
  <+> "of"
  <+> prettyContinuation vs pr bs
prettyG _  pr (App _n f xs) = hsep $ prettyFunc pr f : prettySorted (imap (fmap Foldable.toList . fmap . pr) xs)
prettyG vs pr (Let bs e)    = prettyLet vs pr False bs e
prettyG vs pr (LetRec bs e) = prettyLet vs pr True bs e
prettyG _ _   Slot          = text "slot"

prettyFunc :: (Sort -> Ref -> Doc) -> Func -> Doc
prettyFunc pr (Ref  r) = pr B r
prettyFunc _  (Con  t) = "<" <> text (show t) <> ">"

rummage :: [a] -> Word64 -> Either a Word64
rummage []     n = Right n
rummage (x:_)  0 = Left x
rummage (_:xs) n = rummage xs (n-1)

push :: Sorted [Doc] -> (Sort -> Ref -> Doc) -> Sort -> Ref -> Doc
push xs f s = either id (f s) . _Stack (rummage (xs^.sort s))

prettyContinuation :: [Doc] -> (Sort -> Ref -> Doc) -> Continuation -> Doc
prettyContinuation vs pr (Continuation bs d) = block $ (f <$> Map.toList bs) ++ ldd
  where
    ldd = maybeToList $ (\c -> "_ ->" <+> prettyG vs pr c) <$> d
    f (t, (n, c)) = "<" <> text (show t) <> ">"
                <+> hsep (prettySorted ws ++ ["->" <+> prettyG rest (push ws pr) c])
      where (ws, rest) = splitSorted n vs

prettySorted :: Sorted [Doc] -> [Doc]
prettySorted cvs = ifoldMap go cvs where
  go cc ws = text (show cc) <> semiBraces ws <$ guard (not (Prelude.null ws))

splitSorted :: Sorted Word64 -> [a] -> (Sorted [a], [a])
splitSorted = runState . traverse (state . splitAt . fromIntegral)

prettyLet :: [Doc] -> (Sort -> Ref -> Doc) -> Bool -> Vector PreClosure -> G -> Doc
prettyLet vs pr rec bs e =
      "let"
  <+> block (zipWith (\w bd -> w <+> "=" <+> bd) ws (Foldable.toList bds))
  <+> "in"
  <+> prettyG rest pr' e
 where
 bl = V.length bs
 (ws, rest) = splitAt bl vs
 pr' = push (Sorted ws [] []) pr
 bds = prettyPreClosure rest (if rec then pr' else pr) <$> bs

prettyPreClosure :: [Doc] -> (Sort -> Ref -> Doc) -> PreClosure -> Doc
prettyPreClosure vs pr (PreClosure cfvs (LambdaForm fa ba up bo))
  = hsep
  $ dfvs : dl : prettySorted cbvs ++ ["->" <+> prettyG rest pr' bo]
  where
    dl = "\\" <> if up then "u" else "n"
    dfvs = semiBraces $ join $ Foldable.toList $ imap (fmap Foldable.toList . fmap . pr) cfvs
    (cbvs, rest) = splitSorted ba vs
    pr' c (Local n)
      | n < index fa c = pr c $ index cfvs c V.! fromIntegral n
      | otherwise = error "PANIC: prettyPreClosure: Bad variable reference"
      where
    pr' c r = push cbvs pr c r
