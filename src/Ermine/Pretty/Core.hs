{-# LANGUAGE PatternGuards #-}

--------------------------------------------------------------------
-- |
-- Copyright :  (c) Dan Doel 2013
-- License   :  BSD3
-- Maintainer:  Dan Doel <dan.doel@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
-- Tools for pretty-printing 'Core' expressions.
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
import Data.Text (unpack)
import Data.Traversable
import Data.Word
import Ermine.Pretty
import Ermine.Pretty.Literal
import Ermine.Pretty.Type
import Ermine.Syntax.Core
import Ermine.Syntax.Head
import Ermine.Syntax.Name

-- | Pretty print JavaLike FFI descrptions
prettyJavaLike :: JavaLike -> Doc
prettyJavaLike (Method _st _cn _mn _args) = text $ "method{..}" -- TODO
prettyJavaLike (Constructor _cn _args)    = text $ "constructor{..}" -- TODO
prettyJavaLike (Value _st _cn _fn)        = text $ "value{..}" -- TODO

prettyForeign :: Foreign -> Doc
prettyForeign (JavaLike j) = prettyJavaLike j
prettyForeign (Unknown s)  = text $ "unknown{" ++ show s ++ "}"

-- | Pretty print a 'HardCore' expression.
prettyHardCore :: Int -> HardCore -> Doc
prettyHardCore _ (Super   i)    = text $ "super{" ++ show i ++ "}"
prettyHardCore _ (Slot    i)    = text $ "slot{"  ++ show i ++ "}"
prettyHardCore _ (Lit     l)    = prettyLiteral l
prettyHardCore _ (PrimOp  s)    = text $ "primop{" ++ show s ++ "}"
prettyHardCore _ (Foreign f)    = prettyForeign f
prettyHardCore n (Error   s)    = parensIf (n>10) . text $ "error " ++ show s
prettyHardCore _ (GlobalId g)   = text $ "global{" ++ show g ++ "}"
prettyHardCore _ (InstanceId i) =
  text ("instance{" ++ unpack (i^.headClass.name))
     <+> hsep ((prettyType ?? (repeat "_") ?? 1000) . bimap (const "_") (const "_")
           <$> i^.headTypeArgs)
      <> text "}"

-- | Pretty print a 'HardCore' expression.
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
 eq l r = l <+> text "=" <+> r
 h bds ed = parensIf (prec>=0) . nest 2 $
              text "let" <+> block (zipWith eq ws bds) <+> text "in" <+> ed
prettyCore (v:vs) prec k (LamDict (Scope e)) =
  coreLam prec [v'] <$> prettyCore vs (-1) k' e
 where
 v' = text v
 k' (B _) _ = pure v'
 k' (F c) p = prettyCore vs p k c
prettyCore vs prec k (AppDict f d) =
  (\df dd -> parensIf (prec > 10) $ df <+> dd)
    <$> prettyCore vs 10 k f
    <*> prettyCore vs 11 k d
prettyCore vs _ k (Dict sups sls) =
  (\xs ys -> text "dict" <> block [text "supers" `eq` block xs, text "slots" `eq` block (zipWith eq slotNames ys)])
    <$> traverse (prettyCore vs 0 k) sups
    <*> traverse (prettyCore rest 0 k' . unscope) sls
 where
  (slotNames,rest) = first (fmap text) $ splitAt (length sls) vs
  eq l r = l <+> text "=" <+> r
  k' (B i) _ = pure $ slotNames !! (fromIntegral i)
  k' (F c) p = prettyCore rest p k c
prettyCore _ _ _ _ = pure $ text "unimplemented"

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
