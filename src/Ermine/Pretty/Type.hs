{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------
-- |
-- Module    :  Ermine.Pretty.Kind
-- Copyright :  (c) Edward Kmett and Dan Doel 2012
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Ermine.Pretty.Type
  (
  -- * Pretty Printing Kinds
    prettyHardType
  , prettyType
  , prettyTypeSchema
  ) where

import Bound
import Control.Applicative
import Ermine.Pretty
import Ermine.Pretty.Kind
import Ermine.Syntax.Global
import Ermine.Syntax.Type
import Data.Foldable
import Data.Semigroup
import Data.Traversable
import qualified Data.ByteString.Char8 as Char8

{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}

bananas :: Doc -> Doc
bananas xs = text "(|" <> xs <> text "|)"

prettyHardType :: HardType -> Doc
prettyHardType (Tuple i) = parens (text (replicate (i-1) ','))
prettyHardType Arrow     = parens "->"
prettyHardType (Con (Global _ Idfix _ _ n) _)       = text (Char8.unpack n)
prettyHardType (Con (Global _ (Prefix _) _ _ n) _)  = parens ("prefix" <+> text (Char8.unpack n))
prettyHardType (Con (Global _ (Postfix _) _ _ n) _) = parens ("postfix" <+> text (Char8.unpack n))
prettyHardType (Con (Global _ _ _ _ n) _)           = parens (text (Char8.unpack n))
prettyHardType (ConcreteRho xs) = bananas (fillSep (punctuate (text ",") (text <$> toList xs)))

-- | Pretty print a 'Type' using a fresh variable supply, an ambient precedence, and
-- helpers for printing free kind and type variables.
--
-- You should have already removed any free variables from the variable set.
prettyType :: Applicative f => Type k a -> [String] -> Int -> (k -> Bool -> f Doc) -> (a -> Int -> f Doc) -> f Doc
prettyType (HardType t) _ _ _ _ = pure $ prettyHardType t
prettyType (Var a) _ d _ kt = kt a d
prettyType (App (App (HardType Arrow) x) y) xs d kk kt =
  combine <$> prettyType x xs 1 kk kt <*> prettyType y xs 0 kk kt
 where combine dx dy = parensIf (d > 0) $ dx <+> text "->" <+> dy
prettyType (App x y) xs d kk kt = (\dx dy -> parensIf (d > 10) (dx <+> dy)) <$> prettyType x xs 10 kk kt <*> prettyType y xs 11 kk kt -- TODO: group this better
prettyType (Loc _ r) xs d kk kt = prettyType r xs d kk kt
prettyType (And cs) xs _ kk kt = go <$> traverse (\c -> prettyType c xs 0 kk kt) cs
  where go [d] = d
        go ds  = parens . fillSep $ punctuate "," ds
prettyType (Exists n ks cs) xs d kk kt = go
    <$> traverse (\k -> prettyKind (fromScope k) False kkk) ks
    <*> prettyType (unscope cs) rvs 0 kkk tkk
  where
    (kvs, (tvs, rvs)) = splitAt (length ks) <$> splitAt n xs
    kkk (B b) _ = pure $ text (kvs !! b)
    kkk (F f) p = kk f p
    tkk (B b) _ = pure $ text (tvs !! b)
    tkk (F f) p = prettyType f rvs p kkk kt
    go tks ds = parensIf (d > 0) $ quantified ds
      where
        consKinds zs
          | n /= 0    = braces (fillSep (text <$> kvs)) : zs
          | otherwise = zs
        quantified zs
          | null ks = zs
          | otherwise = hsep ("exists" : consKinds (zipWith (\tv tk -> parens (text tv <+> ":" <+> tk)) tvs tks)) <> "." <+> zs
prettyType (Forall n ks cs bdy) xs d kk kt = go
    <$> traverse (\k -> prettyKind (fromScope k) False kkk) ks
    <*> prettyType (unscope cs) rvs 0 kkk tkk
    <*> prettyType (unscope bdy) rvs 0 kkk tkk
  where
    (kvs, (tvs, rvs)) = splitAt (length ks) <$> splitAt n xs
    kkk (B b) _ = pure $ text (kvs !! b)
    kkk (F f) p = kk f p
    tkk (B b) _ = pure $ text (tvs !! b)
    tkk (F f) p = prettyType f rvs p kkk kt
    go tks ds t = parensIf (d > 0) $ quantified $ constrained t
      where
        consKinds zs
          | n /= 0 = braces (fillSep (text <$> kvs)) : zs
          | otherwise = zs
        quantified zs
          | n == 0 && null ks = zs
          | otherwise = hsep ("forall" : consKinds (zipWith (\tv tk -> parens (text tv <+> ":" <+> tk)) tvs tks)) <> "." <+> zs
        constrained zs
          | isTrivialConstraint . fromScope $ cs = zs
          | otherwise                            = ds <+> "=>" <+> zs

prettyTypeSchema :: Applicative f
                 => Scope Int (TK k) a -> (Int, Int)
                 -> [String] -> (k -> Bool -> f Doc) -> (a -> Int -> f Doc) -> f Doc
prettyTypeSchema (Scope s) (kn, tn) vs kk kt = prettyType s vs' 0 kk' kt'
 where
 (kvs, (tvs, vs')) = splitAt tn <$> splitAt kn vs
 kk' (B i) _ = pure . text $ kvs !! i
 kk' (F k) b = kk k b
 kt' (B i) _ = pure . text $ tvs !! i
 kt' (F t) p = prettyType t vs' p kk' kt
