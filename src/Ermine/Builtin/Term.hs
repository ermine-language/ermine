{-# LANGUAGE PatternGuards #-}

--------------------------------------------------------------------
-- |
-- Module    :  Ermine.Builtin.Term
-- Copyright :  (c) Edward Kmett, Dan Doel 2012-2013
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
-- Smart builders for convenient building of terms.
--------------------------------------------------------------------

module Ermine.Builtin.Term ( lam
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
                           , shapely
                           , finalizeBody
                           ) where

import Bound
import Control.Applicative
import Control.Comonad
import Data.ByteString.Char8 hiding (elemIndex, unzip, all, length)
import Data.List as List
import Data.Monoid
import Ermine.Builtin.Pat
import Ermine.Diagnostic
import Ermine.Syntax.Global
import Ermine.Syntax.Pat
import Ermine.Syntax.Term

lam :: Eq v => Binder v [Pat t] -> Term t v -> Term t v
lam (Binder vs ps) = Lam ps . abstract (`List.elemIndex` vs)

-- | Construct a builtin term 'DataCon' for a given 'global' in the @\"ermine\"@ package
dataCon :: Fixity -> String -> String -> Term t v
dataCon f m n = HardTerm . DataCon $ global f (pack "ermine") (pack m) (pack n)

let_ :: Eq v => Binder v [Binding t v] -> Term t v -> Term t v
let_ (Binder vs ds) b = Let ds $ abstract (`List.elemIndex` vs) b

data PreBinding t v = PreBinding Rendering (BindingType t) [PreBody t v]

data PreBody t v = PreBody (Binder v [Pat t]) (Term t v) (Binder v [Binding t v])

body :: Binder v [Pat t] -> Term t v -> Binder v [Binding t v] -> PreBody t v
body = PreBody

shapely :: [PreBody t v] -> Bool
shapely [    ] = False
shapely (b:bs) = all (\b' -> plength b == plength b') bs
  where plength (PreBody ps _ _) = length $ extract ps

implicit :: [PreBody t v] -> PreBinding t v
implicit bs = PreBinding mempty Implicit bs

explicit :: t -> [PreBody t v] -> PreBinding t v
explicit t bs = PreBinding mempty (Explicit t) bs

bindings :: Eq v => [(v, PreBinding t v)] -> Binder v [Binding t v]
bindings = extend finalizeBindings . uncurry Binder . unzip

finalizeBindings :: Eq v => Binder v [PreBinding t v] -> [Binding t v]
finalizeBindings (Binder vs pbs) = finalizeBinding vs <$> pbs

finalizeBinding :: Eq v => [v] -> PreBinding t v -> Binding t v
finalizeBinding vs (PreBinding r bt bs) = Binding r bt $ finalizeBody vs <$> bs

finalizeBody :: Eq v => [v] -> PreBody t v -> Body t v
finalizeBody ns (PreBody (Binder vs ps) c (Binder ws wh)) =
  Body ps (abstract f c) (fmap av <$> wh)
 where
 av x | Just i <- elemIndex x vs = B i
      | otherwise                = F x
 f x | Just i <- elemIndex x ws = Just (W i)
     | Just i <- elemIndex x vs = Just (P i)
     | Just i <- elemIndex x ns = Just (D i)
     | otherwise                = Nothing
