{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett and Dan Doel 2012
-- License   :  BSD2
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
import Bound.Scope
import Control.Applicative
import Control.Comonad
import Control.Lens
import Data.Set (member, union, Set)
import Data.Set.Lens
import Data.Foldable
import Data.Semigroup
import Data.Text (unpack)
import Ermine.Pretty
import Ermine.Pretty.Kind
import Ermine.Syntax.Global
import Ermine.Syntax.Hint
import Ermine.Syntax.Kind (kindVars, Kind)
import Ermine.Syntax.Type

{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}

bananas :: Doc -> Doc
bananas xs = text "(|" <> xs <> text "|)"

prettyHardType :: HardType -> Doc
prettyHardType (Tuple i) = parens (text (replicate (i-1) ','))
prettyHardType Arrow     = parens "->"
prettyHardType (Con (Global _ Idfix _ n) _)       = text (unpack n)
prettyHardType (Con (Global _ (Prefix _) _ n) _)  = parens ("prefix" <+> text (unpack n))
prettyHardType (Con (Global _ (Postfix _) _ n) _) = parens ("postfix" <+> text (unpack n))
prettyHardType (Con (Global _ _ _ n) _)           = parens (text (unpack n))
prettyHardType (ConcreteRho xs) = bananas (fillSep (punctuate (text ",") (text . unpack <$> toList xs)))

skvs :: Ord k => [(Hint, Scope b Kind k)] -> Set k
skvs = setOf (traverse.traverse.traverseScope pure)

tkvs :: Ord k => Scope b (TK k) t -> Set k
tkvs (Scope e) = setOf (kindVars.traverse) e
         `union` setOf (traverse.traverse.kindVars.traverse) e

-- | Pretty print a 'Type' using a fresh variable supply and an ambient precedence.
--
-- You should have already removed any free variables from the variable set.
prettyType :: Type String String -> [String] -> Int -> Doc
prettyType (HardType t) _  _ = prettyHardType t
prettyType (Var nm)     _  _ = text nm
prettyType (Loc _ r)    vs d = prettyType r vs d
prettyType (App (App (HardType Arrow) x) y) vs d =
  parensIf (d > 0) $ prettyType x vs 1 <+> text "->" <+> prettyType y vs 0
prettyType (App f x)    vs d =
  parensIf (d > 10) $ prettyType f vs 10 <+> prettyType x vs 11
prettyType (And cs)     vs d = case cs of
  [c] -> prettyType c vs d
  _   -> tupled $ map (prettyType ?? vs ?? 0) cs
prettyType (Exists n ks cs) vs d =
  parensIf (d > 0) $ quantified (prettyType (inst cs) rvs 0)
 where
 kAvoid = member ?? skvs ks `union` tkvs cs
 tAvoid = member ?? setOf (traverseScope pure) cs
 (kvs, (tvs, rvs)) = chooseNames tAvoid (fmap fst ks) <$> chooseNames kAvoid n vs

 tks = map (\k -> prettyKind (instantiate (pure . (kvs!!)) $ extract k) False) ks

 inst = instantiateKindVars kvs . instantiate (pure . (tvs!!))

 consKinds | null kvs  = id
           | otherwise = (braces (fillSep (text <$> kvs)) :)

 quantified zs
   | null tvs  = zs
   | otherwise =
      hsep ("exists" :
              consKinds (zipWith (\tv tk -> parens (text tv <+> colon <+> tk))
                                 tvs tks)) <> dot
        <+> zs
prettyType (Forall n ks cs bdy) vs d =
  parensIf (d > 0) . quantified . constrained $ prettyType (inst bdy) rvs 0
 where
 kAvoid = member ?? skvs ks
            `union` tkvs cs
            `union` tkvs bdy
 tAvoid = member ?? setOf (traverseScope pure) cs `union` setOf (traverseScope pure) bdy

 (kvs, (tvs, rvs)) = chooseNames tAvoid (fmap fst ks) <$> chooseNames kAvoid n vs

 tks = map (\k -> prettyKind (instantiate (pure . (kvs!!)) $ extract k) False) ks

 inst = instantiateKindVars kvs . instantiate (pure . (tvs!!))

 consKinds | null kvs  = id
           | otherwise = (braces (fillSep (text <$> kvs)) :)

 quantified zs
   | null tvs && null kvs = zs
   | otherwise =
      hsep ("forall" :
              consKinds (zipWith (\tv tk -> parens (text tv <+> colon <+> tk))
                                 tvs tks)) <> dot
        <+> zs
 constrained zs
   | isTrivialConstraint . fromScope $ cs = zs
   | otherwise                            = prettyType (inst cs) rvs 0 <+> "=>" <+> zs

prettyTypeSchema :: Scope Int (TK String) String -> ([Hint], [Hint])
                 -> [String] -> Doc
prettyTypeSchema s (kn, tn) vs = prettyType (inst s) vs' 0
 where
 kAvoid = member ?? tkvs s
 tAvoid = member ?? setOf (traverseScope pure) s

 (kvs, (tvs, vs')) = chooseNames tAvoid tn <$> chooseNames kAvoid kn vs
 inst = instantiateKindVars kvs . instantiate (pure . (tvs!!))
