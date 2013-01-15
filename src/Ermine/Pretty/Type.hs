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
  ) where

import Bound
import Control.Applicative
import Ermine.Syntax.Kind (Kind)
import Ermine.Pretty
import Ermine.Pretty.Kind
import Ermine.Syntax.Global
import Ermine.Syntax.Type
import Data.Foldable
import Data.Traversable
import qualified Data.ByteString.Char8 as Char8

bananas :: Doc a -> Doc a
bananas xs = text "(|" <> xs <> text "|)"

prettyHardType :: HardType -> Doc a
prettyHardType (Tuple i) = parens (text (replicate (i-1) ','))
prettyHardType Arrow     = parens ("->")
prettyHardType (Con (Global _ Idfix _ _ n) _)       = text (Char8.unpack n)
prettyHardType (Con (Global _ (Prefix _) _ _ n) _)  = parens ("prefix" <+> text (Char8.unpack n))
prettyHardType (Con (Global _ (Postfix _) _ _ n) _) = parens ("postfix" <+> text (Char8.unpack n))
prettyHardType (Con (Global _ _ _ _ n) _)           = parens (text (Char8.unpack n))
prettyHardType (ConcreteRho xs) = bananas (fillSep (punctuate (text ",") (text <$> toList xs)))

fromTK :: Scope Int (TK k) a -> Type (Var Int (Kind k)) (Var Int a)
fromTK = runTK . fromScope

-- | Pretty print a 'Kind', using a fresh kind variable supply and a helper to print free variables
--
-- You should have already removed any free variables from the variable set.
prettyType :: Applicative f => Type k a -> [String] -> Int -> (k -> Bool -> f (Doc b)) -> (a -> Int -> f (Doc b)) -> f (Doc b)
prettyType (HardType t) _ _ _ _ = pure $ prettyHardType t
prettyType (Var a) _ d _ kt = kt a d
prettyType (App x y) xs d kk kt = (\dx dy -> parensIf (d > 10) (dx <+> dy)) <$> prettyType x xs 10 kk kt <*> prettyType y xs 11 kk kt -- TODO: group this better
prettyType (Loc _ r) xs d kk kt = prettyType r xs d kk kt
prettyType (Exists _tks _cs) _xs _d _kk _kt = pure "exists..."
prettyType (Forall n ks cs bdy) xs d kk kt = go
    <$> traverse (\k -> prettyKind (unscope k) False kkk) ks
    <*> traverse (\c -> prettyType (fromTK c) rvs 0 kkk tkk) cs
    <*> prettyType (fromTK bdy) rvs 0 kkk tkk
  where
    (kvs, (tvs, rvs)) = splitAt (length ks) <$> splitAt n xs

    -- Var Int (Kind k) -> Bool -> f (Doc b)
    kkk (B b) _ = pure $ text (kvs !! b)
    kkk (F f) p = prettyKind f p kk

    -- Var Int a -> Int -> f (Doc b)
    tkk (B b) _ = pure $ text (tvs !! b)
    tkk (F f) p = kt f p

    go tks ds t = parensIf (d > 0) $ quantified $ constrained t
      where
        consKinds zs
          | n /= 0 = braces (fillSep (text <$> kvs)) : zs
          | otherwise = zs
        quantified zs
          | n == 0 && null ks = zs
          | otherwise = hsep ("forall" : consKinds (zipWith (\tv tk -> parens (text tv <+> ":" <+> tk)) tvs tks)) <> "." <+> zs
        constrained zs = case compare (length ds) 1 of
          LT -> zs
          EQ -> head ds <+> "=>" <+> zs
          GT -> parens (fillSep (punctuate "," ds)) <+> "=>" <+> zs

{-
  | Exists [Kind k] [Scope Int (Type k) a]
  deriving (Show, Functor, Foldable, Traversable)
-}
