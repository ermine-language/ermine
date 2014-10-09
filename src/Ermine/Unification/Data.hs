--------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett and Dan Doel 2013-2014
-- License   :  BSD2
-- Maintainer:  Dan Doel <dan.doel@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Ermine.Unification.Data
  ( DataCheck(DataCheck)
  , instantiateDataType
  , dataCheckKind
  , generalizeDataCheck
  , zonkDataCheck
  , zonkDataType
  , zonkConstructor
  ) where

import Bound
import Bound.Scope
import Bound.Var
import Control.Applicative
import Control.Lens
import Data.List (elemIndex, nub)
import Data.Monoid
import Data.Maybe
import Data.Text (Text)
import Data.Traversable
import Data.Void
import Ermine.Syntax
import Ermine.Syntax.Constructor
import Ermine.Syntax.Data
import Ermine.Syntax.Global
import Ermine.Syntax.Hint
import Ermine.Syntax.Kind
import Ermine.Syntax.Name
import Ermine.Syntax.Type
import Ermine.Unification.Meta

data DataCheck s = DataCheck
                 { _dcname :: Global
                 , _dctparams :: [(Hint, KindM s)]
                 , _dcconstrs :: [Constructor (MetaK s) (Var Int Text)]
                 }

instance HasGlobal (DataCheck s) where
  global f (DataCheck g ts cs) = f g <&> \g' -> DataCheck g' ts cs

instance HasName (DataCheck s) where
  name = global.name

instantiateDataType :: DataType () Text -> M s (Schema (MetaK s), DataCheck s)
instantiateDataType dt = do
  dt'@(DataType g ks ts cs) <- kindVars (\_ -> newShallowMeta 0 False Nothing) dt
  mks <- for ks $ newMeta False
  tks <- for ts $ \(h,_) -> (,) h . pure <$> newShallowMeta 0 False Nothing
  return ( dataTypeSchema dt'
         , DataCheck g tks $ over kindVars (unvar (mks!!) id) <$> cs
         )

dataCheckKind :: DataCheck s -> KindM s
dataCheckKind (DataCheck _ ts _) = foldr ((~>) . snd) star ts

generalizeDataCheck :: DataCheck s -> M s (DataType k Text)
generalizeDataCheck dc = abstr <$> zonkDataCheck dc
 where
 abstr (DataCheck nm ts cs) = DataType nm khs ts' cs'
  where
  ks = nub $ toListOf (traverse.traverse.kindVars) ts <> toListOf kindVars cs
  khs = map (^.metaHint) ks
  bk = B . fromMaybe (error "generalizeDataCheck: impossible") . (`elemIndex` ks)
  bindAll x = over kindVars bk x
  ts' = (fmap.fmap) (toScope . bindAll) ts
  cs' = fmap bindAll cs

zonkDataCheck :: DataCheck s -> M s (DataCheck s)
zonkDataCheck (DataCheck nm ts cs) = do
  ts' <- (traverse.traverse) zonk_ ts
  cs' <- traverse zc cs
  return $ DataCheck nm ts' cs'
 where
 zc :: Constructor (MetaK s) (Var Int a) -> M s (Constructor (MetaK s) (Var Int a))
 zc (Constructor tg ks cts fs) = do
   cts' <- (traverse.traverse) (\s -> (>>>= id) <$> traverseScope pure (zonk_.pure) s) cts
   fs' <- traverse (transverseScope (\tk -> bindTK id <$> kindVars (traverse (zonk_.pure)) tk)) fs
   pure $ Constructor tg ks cts' fs'

zonkDataType :: DataType (MetaK s) Void -> M s (DataType (MetaK s) Void)
zonkDataType dt = constructors zonkConstructor =<< typeParameters (traverse zonkScope_) dt

zonkConstructor :: Constructor (Var Int (MetaK s)) (Var Int a)
                -> M s (Constructor (Var Int (MetaK s)) (Var Int a))
zonkConstructor (Constructor tg ks ts fs) = do
  ts' <- (traverse . traverse) (\s -> (>>>= id) <$> traverseScope pure zk s) ts
  fs' <- traverse (transverseScope (\tk -> bindTK id <$> kindVars (traverse zk) tk)) fs
  pure $ Constructor tg ks ts' fs'
 where
 zk :: Var Int (MetaK s) -> M s (Kind (Var Int (MetaK s)))
 zk (B i) = pure . pure $ B i
 zk (F v) = fmap (fmap F) . zonk_ $ pure v
