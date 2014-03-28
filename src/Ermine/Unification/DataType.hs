
--------------------------------------------------------------------
-- |
-- Copyright :  (c) Dan Doel 2013
-- License   :  BSD3
-- Maintainer:  Dan Doel <dan.doel@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Ermine.Unification.DataType
  ( zonkDataType
  , zonkConstructor
  ) where

import Bound
import Bound.Scope
import Control.Applicative
import Data.Traversable
import Data.Void
import Ermine.Syntax.Constructor
import Ermine.Syntax.DataType
import Ermine.Syntax.Kind
import Ermine.Syntax.Type
import Ermine.Unification.Meta


zonkDataType :: DataType (MetaK s) Void -> M s (DataType (MetaK s) Void)
zonkDataType dt = constructors zonkConstructor =<< typeParameters (traverse zonkScope_) dt

zonkConstructor :: Constructor (Var Int (MetaK s)) (Var Int Void)
                -> M s (Constructor (Var Int (MetaK s)) (Var Int Void))
zonkConstructor (Constructor tg ks ts fs) = do
  ts' <- (traverse . traverse) (\s -> (>>>= id) <$> traverseScope pure zk s) ts
  fs' <- traverse (transverseScope (\tk -> bindTK id <$> kindVars (traverse zk) tk)) fs
  pure $ Constructor tg ks ts' fs'
 where
 zk :: Var Int (MetaK s) -> M s (Kind (Var Int (MetaK s)))
 zk (B i) = pure . pure $ B i
 zk (F v) = fmap (fmap F) . zonk_ $ pure v
