{-# LANGUAGE PatternGuards #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett, Dan Doel 2012-2013
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
-- Smart builders for convenient building of terms.
--------------------------------------------------------------------
module Ermine.Builtin.Term
  ( lam
  , dataCon
  , let_
  , implicit
  , explicit
  , bindings
  , PreBinding(..)
  , finalizeBindings
  , finalizeBinding
  , PreBody(..)
  , body
  , gbody
  , shapely
  , finalizeBody
  ) where

import Bound
import Control.Applicative
import Control.Comonad
import Data.Text (pack)
import Data.List as List
import Data.Monoid
import Data.Void
import Ermine.Builtin.Pattern
import Ermine.Diagnostic
import Ermine.Syntax.Global
import Ermine.Syntax.ModuleName
import Ermine.Syntax.Pattern
import Ermine.Syntax.Term
import Ermine.Syntax.Type as Type

lam :: Eq v => Binder v [Pattern t] -> Term t v -> Term t v
lam (Binder vs ps) = Lam ps . abstract (`List.lookup` zip vs (manyPaths ps))

-- | Construct a builtin term 'DataCon' for a given 'global' in the @\"ermine\"@ package
dataCon :: Fixity -> String -> String -> Type Void Void -> Term t v
dataCon f m n t = HardTerm (DataCon (glob f (mkModuleName_ m) (pack n)) t)

let_ :: Eq v => Binder v [Binding t v] -> Term t v -> Term t v
let_ (Binder vs ds) b = Let ds $ abstract (\i -> fromIntegral <$> List.elemIndex i vs) b

data PreBinding t v = PreBinding Rendering (BindingType t) [PreBody t v]

data PreBody t v = PreBody (Binder v [Pattern t]) (Guarded (Term t v)) (Binder v [Binding t v])

body :: Binder v [Pattern t] -> Term t v -> Binder v [Binding t v] -> PreBody t v
body ps = PreBody ps . Unguarded

gbody :: Binder v [Pattern t] -> [(Term t v, Term t v)] -> Binder v [Binding t v] -> PreBody t v
gbody ps = PreBody ps . Guarded

shapely :: [PreBody t v] -> Bool
shapely [    ] = False
shapely (b:bs) = all (\b' -> plength b == plength b') bs
  where plength (PreBody ps _ _) = length $ extract ps

implicit :: [PreBody t v] -> PreBinding t v
implicit = PreBinding mempty Implicit

explicit :: t -> [PreBody t v] -> PreBinding t v
explicit = PreBinding mempty . Explicit

bindings :: Eq v => [(v, PreBinding t v)] -> Binder v [Binding t v]
bindings = extend finalizeBindings . uncurry Binder . unzip

finalizeBindings :: Eq v => Binder v [PreBinding t v] -> [Binding t v]
finalizeBindings (Binder vs pbs) = finalizeBinding vs <$> pbs

finalizeBinding :: Eq v => [v] -> PreBinding t v -> Binding t v
finalizeBinding vs (PreBinding r bt bs) = Binding bt $ Bodies r $ finalizeBody vs <$> bs

finalizeBody :: Eq v => [v] -> PreBody t v -> Body t v
finalizeBody ns (PreBody (Binder vs ps) gs (Binder ws wh)) =
  Body ps (abstract f <$> gs) (fmap av <$> wh)
 where
 vp = zip vs $ manyPaths ps
 av x | Just p <- lookup x vp    = B (WherePat p)
      | Just i <- elemIndex x ns = B . WhereDecl $ fromIntegral i
      | otherwise                = F x
 f x | Just i <- elemIndex x ws = Just . BodyWhere $ fromIntegral i
     | Just p <- lookup    x vp = Just (BodyPat p)
     | Just i <- elemIndex x ns = Just . BodyDecl $ fromIntegral i
     | otherwise                = Nothing
