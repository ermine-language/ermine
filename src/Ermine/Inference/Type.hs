{-# LANGUAGE CPP #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett 2011-2012
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Ermine.Inference.Type
  ( matchFunType
  , inferType
  ) where

import Bound
import Control.Applicative
import Control.Monad.Writer.Strict
import Ermine.Builtin.Type
import Ermine.Syntax
import Ermine.Syntax.Literal
import Ermine.Syntax.Core as Core
import Ermine.Syntax.Term as Term
import Ermine.Syntax.Type as Type
import Ermine.Syntax.Kind as Kind hiding (Var)
import Ermine.Inference.Witness
import Ermine.Unification.Kind
import Ermine.Unification.Type
import Ermine.Unification.Meta

type WitnessM s = Witness (MetaK s) (MetaT s)

matchFunType :: TypeM s -> M s (TypeM s, TypeM s)
matchFunType (Type.App (Type.App (HardType Arrow) a) b) = return (a, b)
matchFunType (HardType _) = fail "not a fun type"
matchFunType t = do
  x <- pure <$> newMeta star
  y <- pure <$> newMeta star
  (x, y) <$ runWriterT (unifyType t (x ~> y))

inferType :: Term (Annot (MetaK s) (MetaT s)) (TypeM s) -> M s (WitnessM s)
inferType (HardTerm t) = inferHardType t
inferType _ = fail "Unimplemented"

inferHardType :: HardTerm -> M s (WitnessM s)
inferHardType (Term.Lit l) = return $ Witness [] [] (literalType l) (HardCore (Core.Lit l))
inferHardType (Term.Tuple n) = do
  vars <- replicateM n $ pure <$> newMeta star
  return $ Witness [] [] (foldr (~>) (tup vars) vars) $
             Core.Lam n (Scope $ Core.Data 0 $ pure . B <$> [0..n-1])
inferHardType _ = fail "Unimplemented"

literalType :: Literal -> Type k a
literalType Int{}    = int
literalType Long{}   = long
literalType Byte{}   = byte
literalType Short{}  = short
literalType String{} = string
literalType Char{}   = char
literalType Float{}  = float
literalType Double{} = double
