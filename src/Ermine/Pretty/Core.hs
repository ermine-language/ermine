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
 ( prettyCore
 , prettyHardCore
 , prettyId
 ) where

import Bound
import Control.Lens
import Control.Applicative
import Data.Bifunctor
import Data.Monoid
import Data.Text.Lens hiding (text)
import Data.Traversable
import Data.Word
import Ermine.Pretty
import Ermine.Pretty.Global
import Ermine.Pretty.Literal
import Ermine.Pretty.Type
import Ermine.Syntax.Convention
import Ermine.Syntax.Core
import Ermine.Syntax.Global
import Ermine.Syntax.Head
import Ermine.Syntax.Id
import Ermine.Syntax.Name

-- | Pretty print JavaLike FFI descrptions
prettyJavaLike :: JavaLike -> Doc
prettyJavaLike (Method _st _cn _mn _args) = text "method{..}" -- TODO
prettyJavaLike (Constructor _cn _args)    = text "constructor{..}" -- TODO
prettyJavaLike (Value _st _cn _fn)        = text "value{..}" -- TODO

prettyForeign :: Foreign -> Doc
prettyForeign (JavaLike j) = prettyJavaLike j
prettyForeign (Unknown s)  = text $ "unknown{" ++ show s ++ "}"

-- | Pretty print a 'HardCore' expression.
prettyHardCore :: Int -> HardCore -> Doc
prettyHardCore _ (Super   i)     = text $ "super{" ++ show i ++ "}"
prettyHardCore _ (Slot    i)     = text $ "slot{"  ++ show i ++ "}"
prettyHardCore _ (Lit     l)     = prettyLiteral l
prettyHardCore _ (Foreign f)     = prettyForeign f
prettyHardCore n (Error   s)     = parensIf (n>10) . text $ "error " ++ show s
prettyHardCore _ (Id      g)     = prettyId g

prettyId :: Id -> Doc
prettyId (GlobalId g)   = prettyGlobal g
prettyId (InstanceId h) = prettyHead h

prettyHead :: Head -> Doc
prettyHead i = text ("instance{" ++ (i^.headClass.name.unpacked))
     <+> hsep ((prettyType ?? repeat "_" ?? 1000) . bimap (const "_") (const "_")
           <$> i^.headTypeArgs)
      <> text "}"

-- | Pretty print a 'HardCore' expression.
prettyCore :: Applicative f
           => [String] -> Int -> (a -> Int -> f Doc) -> Core Convention a -> f Doc
prettyCore _  prec k (Var v) = k v prec
prettyCore _  prec _ (HardCore h) = pure $ prettyHardCore prec h
prettyCore vs prec k (Data cc tg g fs) =
  parensIf (prec > 10) . coreData cc tg g <$> traverse (prettyCore vs 11 k) fs
prettyCore vs _    k (Prim cc r g fs) =
  corePrim cc r g <$> traverse (prettyCore vs 11 k) fs
prettyCore vs prec k (App _ f x) =
  (\df dx -> parensIf (prec>10) $ df <+> dx)
    <$> prettyCore vs 10 k f <*> prettyCore vs 11 k x
prettyCore vs prec k (Lam cc (Scope e)) =
  coreLam cc prec ws <$> prettyCore rest (-1) k' e
 where
 (ws, rest) = first (fmap text) $ splitAt (length cc) vs
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
 branches = for (itoList m) $ \(tg, Match cc g (Scope b)) ->
   let (ws,rest) = first (fmap text) $ splitAt (length cc) vs
       k' (B 0) _ = pure dv
       k' (B i) _ = pure $ ws !! fromIntegral (i-1)
       k' (F c) p = prettyCore rest p k c
    in (\bd -> nest 2 $ coreData cc tg g ws <+> text "->" <+> bd)
          <$> prettyCore rest (-1) k' b

prettyCore vs prec k (Let bs e) = h <$> traverse pc bs <*> pc e
 where
 pc = prettyCore rest (-1) k' . unscope
 n = length bs
 (ws,rest) = first (fmap text) $ splitAt n vs
 k' (B i) _ = pure $ ws !! fromIntegral i
 k' (F c) p = prettyCore rest p k c
 eq l r = l <+> text "=" <+> r
 h bds ed = parensIf (prec>=0) . nest 2 $
              text "let" <+> block (zipWith eq ws bds) <+> text "in" <+> ed
prettyCore vs _ k (Dict sups sls) =
  (\xs ys -> text "dict" <> block [text "supers" `eq` block xs, text "slots" `eq` block (zipWith eq slotNames ys)])
    <$> traverse (prettyCore vs 0 k) sups
    <*> traverse (prettyCore rest 0 k' . unscope) sls
 where
  (slotNames,rest) = first (fmap text) $ splitAt (length sls) vs
  eq l r = l <+> text "=" <+> r
  k' (B i) _ = pure $ slotNames !! fromIntegral i
  k' (F c) p = prettyCore rest p k c
prettyCore [] _ _ _ = error "panic: prettyCore ran out of variable names"

-- \{a,b} [C,C]->C body
coreLam :: [Convention] -> Int -> [Doc] -> Doc -> Doc
coreLam cc prec ws e = parensIf (prec>=0) $
  text "\\" <> encloseSep lbrace rbrace comma ws <+> encloseSep lbracket rbracket comma (text.show<$>cc) <> text "->" <+> e

-- Nothing<0,[]>
-- Just<1,[C]> a
coreData :: [Convention] -> Word64 -> Global -> [Doc] -> Doc
coreData cc tg g fds = text (g^.name.unpacked) <> text "<" <> int (fromIntegral tg) <> text "," <> text (show cc) <> text ">" <+> hsep fds

corePrim :: [Convention] -> Convention -> Global -> [Doc] -> Doc
corePrim cc tg g fds = text (g^.name.unpacked) <> text "<" <> text (show tg) <> text "," <> text (show cc) <> text ">" <+> hsep fds

coreCase :: Doc -> Int -> Doc -> [Doc] -> Maybe Doc -> Doc
coreCase dv prec de dbs mdd = parensIf (prec>=0) $
  nest 2 $ text "case" <+> de <+> text "of" <+> dv <$$> coreBlock dbs'
 where dbs' | Just dd <- mdd = dbs ++ [text "_ ->" <+> dd]
            | otherwise      = dbs

coreCaseLit :: Bool -> Int -> Doc -> [Doc] -> Maybe Doc -> Doc
coreCaseLit cc prec de dbs mdd = parensIf (prec>=0) $
  nest 2 $ text (if cc then "case[N]" else "case[U]") <+> de <+> text "of" <$$> coreBlock dbs'
 where dbs' | Just dd <- mdd = dbs ++ [text "_ ->" <+> dd]
            | otherwise      = dbs

coreBlock :: [Doc] -> Doc
coreBlock [    ] = text "{}"
coreBlock (d:ds) = vsep (lbrace <+> d : map (semi <+>) ds) <> line <> rbrace
