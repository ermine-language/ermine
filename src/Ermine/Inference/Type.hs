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
  ) where

import Control.Applicative
import Control.Monad.Writer.Strict
import Ermine.Syntax
import Ermine.Syntax.Type
import Ermine.Syntax.Kind as Kind hiding (Var)
import Ermine.Unification.Type
import Ermine.Unification.Meta

matchFunType :: TypeM s -> M s (TypeM s, TypeM s)
matchFunType (App (App (HardType Arrow) a) b) = return (a, b)
matchFunType (HardType _) = fail "not a fun type"
matchFunType t = do
  x <- Var <$> newMeta star
  y <- Var <$> newMeta star
  (x, y) <$ runWriterT (unifyType t (x ~> y))
