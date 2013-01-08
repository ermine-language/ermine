module Ermine.Type.Inference where

import Bound
import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.ST
import Control.Monad.ST.Class
import Control.Monad.Writer.Strict
import Data.Foldable
import Data.IntSet as IntSet
import Data.Maybe
import Data.STRef
-- import qualified Ermine.Kind as Kind
import           Ermine.Kind.Inference
import           Ermine.Meta
-- import qualified Ermine.Term as Term
import           Ermine.Type as Type
-- import qualified Ermine.Core as Core

unifyType :: IntSet -> TypeM s -> TypeM s -> M s (Maybe (TypeM s))
unifyType is t1 t2 = do
  t1' <- semiprune t1
  t2' <- semiprune t2
  go t1' t2'
  where
    go (Var tv1)                 (Var tv2)                 | tv1 == tv2 = return Nothing
    go x@(Var (Meta _ i r d u)) y@(Var (Meta _ j s e v)) = do
       -- union-by-rank
       m <- liftST $ readSTRef u
       n <- liftST $ readSTRef v
       Just <$> if m <= n
         then unifyTV is i r d y (let m' = m + 1 in m' `seq` writeSTRef u m')
         else unifyTV is j s e x (let n' = n + 1 in n' `seq` writeSTRef u n')
    go (Var (Meta _ i r d u)) t                       = Just <$> unifyTV is i r d t (bumpRank u)
    go t                      (Var (Meta _ i r d u))  = Just <$> unifyTV is i r d t (bumpRank u)
    go (App f x)              (App g y)               = merge <$> unifyType is f g <*> unifyType is x y where
      merge Nothing  Nothing  = Nothing
      merge (Just h) Nothing  = Just (App h x)
      merge Nothing  (Just z) = Just (App f z)
      merge (Just h) (Just z) = Just (App h z)
    go (Loc l s)              t                       = fmap (Loc l) <$> unifyType is s t
    go s                      (Loc _ t)               = unifyType is s t
    go Exists{}               _                       = fail "unifyType: existential"
    go _                      Exists{}                = fail "unifyType: existential"
    go (Forall m xs [] _a)     (Forall n ys [] _b)
      | m /= n                 = fail "unifyType: forall: mismatched kind arity"
      | length xs /= length ys = fail "unifyType: forall: mismatched type arity"
      | otherwise = do
      ks <- replicateM m $ newSkolem ()
      let nxs = instantiate (pure . (ks!!)) <$> xs
          nys = instantiate (pure . (ks!!)) <$> ys
      _sks <- for_ (zip nxs nys) $ \(x,y) -> do
        k <- unifyKind mempty x y
        newSkolem k
      error "TODO"
    go (HardType x) (HardType y) | x == y = return Nothing
    -- go _ _ = fail "type mismatch"

unifyTV :: IntSet -> Int -> STRef s (Maybe (TypeM s)) -> STRef s Depth -> TypeM s -> ST s () -> M s (TypeM s)
unifyTV is i r d t bump = liftST (readSTRef r) >>= \ mt1 -> case mt1 of
  Just j | is^.ix i -> fail "type occurs"
         | otherwise -> fromMaybe j <$> unifyType (IntSet.insert i is) j t
  Nothing -> case t of
    Var (Meta _ _ _ e _) -> liftST $ do -- this has been semipruned so its not part of a chain
      bump
      writeSTRef r (Just t)
      depth1 <- readSTRef d
      depth2 <- readSTRef e
      t <$ when (depth2 > depth1) (writeSTRef e depth1)
    _ -> do
      depth1 <- liftST $ readSTRef d
      if depth1 < maxBound
        then runWriterT (adjustRank (IntSet.insert i is) depth1 t) <&> \ (t2, Any b) -> if b then t2 else t
        else return t -- we're done, zonk remaining cycles later

adjustRank :: IntSet -> Depth -> TypeM s -> WriterT Any (M s) (TypeM s)
adjustRank is depth1 = fmap join . traverse go where
  go v@(Meta _ j s d _)
    | is^.ix j  = fail "type occurs"
    | otherwise = liftST (readSTRef s) >>= \ mt -> case mt of
    Just t -> do -- we found a target
      tell (Any True) -- we're zonking
      t' <- adjustRank (IntSet.insert j is) depth1 t -- chase children
      liftST $ t' <$ writeSTRef s (Just t')
    Nothing -> liftST $ do -- we didn't find a target, adjust the depth of this variable
      depth2 <- readSTRef d
      Var v <$ when (depth2 > depth1) (writeSTRef d depth1) -- boring
  go v = return (pure v)

{-
-- | This doubles as our occurs check
--
-- TODO: switch to M s (Maybe (TypeM s)) to avoid boring updates
adjustRank :: IntSet -> Depth -> TypeM s -> M s (Maybe (TypeM s))
adjustRank is depth1 = go2 where
       traverse go :: TypeM s -> M s (TypeM s (Maybe (Type m)))
  go2 = fmap join . traverse go

  go :: MetaT s -> M s (Maybe (TypeM s))
  go v@(Meta _ j s d _)
    | is^.ix j  = fail "type occurs"
    | otherwise = liftST (readSTRef s) >>= \ mt -> case mt of
    Just t -> adjustRank (IntSet.insert j is) t >>= \mt' -> case mt' of -- we found a target, so zonk as we go
        Just t' -> mt' <$ liftST (writeSTRef s mt') -- we made interesting edits pass them up
        Nothing -> return mt                        -- we changed nothing. give back ourself
    Nothing -> do -- we didn't find a target, adjust the depthof this variable
      depth2 <- readSTRef d
      pure v <$ when (depth2 > depth1) (writeSTRef d depth1)
-}
