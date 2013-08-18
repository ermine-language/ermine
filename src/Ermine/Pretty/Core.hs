{-# LANGUAGE PatternGuards #-}

--------------------------------------------------------------------
-- |
-- Copyright :  (c) Dan Doel 2013
-- License   :  BSD3
-- Maintainer:  Dan Doel <dan.doel@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------

module Ermine.Pretty.Core
 ( prettyHardCore
 , prettyCore
 ) where

import Bound
import Control.Lens
import Control.Applicative
import Data.Bifunctor
import Data.Monoid
import Data.Traversable
import Data.Word
import Ermine.Pretty
import Ermine.Pretty.Literal
import Ermine.Syntax.Core

prettyHardCore :: Int -> HardCore -> Doc
prettyHardCore _ (Super i) = text $ "super{" ++ show i ++ "}"
prettyHardCore _ (Slot  i) = text $ "slot{" ++ show i ++ "}"
prettyHardCore _ (Lit   l) = prettyLiteral l
prettyHardCore n (Error s) = parensIf (n>10) . text $ "error " ++ show s

--   | Dict { supers :: [Core a], slots :: [Scope Int Core a] }
--   | LamDict !(Scope () Core a)
--   | AppDict !(Core a) !(Core a)
prettyCore :: Applicative f
           => [String] -> Int -> (a -> Int -> f Doc) -> Core a -> f Doc
prettyCore _  prec k (Var v) = k v prec
prettyCore _  prec _ (HardCore h) = pure $ prettyHardCore prec h
prettyCore vs _    k (Data t fs) =
  coreData t <$> traverse (prettyCore vs (-1) k) fs
prettyCore vs prec k (App f x) =
  (\df dx -> parensIf (prec>10) $ df <+> dx)
    <$> prettyCore vs 10 k f <*> prettyCore vs 11 k x
prettyCore vs prec k (Lam n (Scope e)) =
  coreLam prec ws <$> prettyCore rest (-1) k' e
 where
 (ws, rest) = first (fmap text) $ splitAt (fromIntegral n) vs
 k' (B i) _ = pure $ ws !! fromIntegral i
 k' (F c) p = prettyCore rest p k c
prettyCore (v:vs) prec k (Case e m d) =
  coreCase dv prec
    <$> prettyCore vs (-1) k e
    <*> branches
    <*> traverse (prettyCore vs (-1) l . unscope) d
 where
 l (B _) _ = pure dv
 l (F c) p = prettyCore vs p k c
 dv = text v
 branches = for (itoList m) $ \(t, (n, Scope b)) ->
   let (ws,rest) = first (fmap text) $ splitAt (fromIntegral n) vs
       k' (B 0) _ = pure dv
       k' (B i) _ = pure $ ws !! (fromIntegral $ i-1)
       k' (F c) p = prettyCore rest p k c
    in (\bd -> nest 2 $ coreData t ws <+> text "->" <+> bd)
          <$> prettyCore rest (-1) k' b
prettyCore vs prec k (Let bs e) = h <$> traverse pc bs <*> pc e
 where
 pc = prettyCore rest (-1) k' . unscope
 n = length bs
 (ws,rest) = first (fmap text) $ splitAt n vs
 k' (B i) _ = pure $ ws !! (fromIntegral i)
 k' (F c) p = prettyCore rest p k c
 h bds ed = parensIf (prec>=0) . nest 2 $
              text "let" <+> block bds <+> text "in" <+> ed
prettyCore _ _ _ _ = pure $ text "unimplemented"
-- prettyCore vs prec k (Dict sups slots) = undefined
-- prettyCore vs prec k (LamDict e) = undefined
-- prettyCore vs prec k (AppDict f d) = undefined

coreLam :: Int -> [Doc] -> Doc -> Doc
coreLam prec ws e = parensIf (prec>=0) $
  text "\\" <> encloseSep lbrace rbrace comma ws <+> text "->" <+> e

coreData :: Word8 -> [Doc] -> Doc
coreData t fds = angles $
  int (fromIntegral t) <> align (cat $ prePunctuate' (text "|") (text ",") fds)

coreCase :: Doc -> Int -> Doc -> [Doc] -> Maybe Doc -> Doc
coreCase dv prec de dbs mdd = parensIf (prec>=0) $
  nest 2 $ text "case" <+> de <+> text "of" <+> dv <$$> coreBlock dbs'
 where dbs' | Just dd <- mdd = text "__DEFAULT__ ->" <+> dd : dbs
            | otherwise      = dbs

coreBlock :: [Doc] -> Doc
coreBlock [    ] = text "{}"
coreBlock (d:ds) = vsep (lbrace <+> d : map (semi <+>) ds) <> line <> rbrace
