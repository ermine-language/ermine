module Ermine.Type.Unification
  ( unifyType
  ) where

import Bound
import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.ST
import Control.Monad.ST.Class
import Control.Monad.Writer.Strict
import Data.IntSet as IntSet
import Data.STRef
import Data.Traversable
import Ermine.Kind.Unification
import Ermine.Meta
import Ermine.Type as Type

unifyType :: IntSet -> TypeM s -> TypeM s -> WriterT Any (M s) (TypeM s)
unifyType is t1 t2 = do
  t1' <- lift $ semiprune t1
  t2' <- lift $ semiprune t2
  go t1' t2'
  where
    go x@(Var tv1)                (Var tv2)              | tv1 == tv2 = return x
    go x@(Var (Meta _ i r d u)) y@(Var (Meta _ j s e v)) = do
       -- union-by-rank
       m <- liftST $ readSTRef u
       n <- liftST $ readSTRef v
       case compare m n of
         LT -> unifyTV is i r d y $ return ()
         EQ -> unifyTV is i r d y $ writeSTRef v $! n + 1
         GT -> unifyTV is j s e x $ return ()
    go (Var (Meta _ i r d _)) t                       = unifyTV is i r d t $ return ()
    go t                      (Var (Meta _ i r d _))  = unifyTV is i r d t $ return ()
    go (App f x)              (App g y)               = App <$> unifyType is f g <*> unifyType is x y
    go (Loc l s)              t                       = Loc l <$> unifyType is s t
    go s                      (Loc _ t)               = unifyType is s t
    go Exists{}               _                       = fail "unifyType: existential"
    go _                      Exists{}                = fail "unifyType: existential"
    go t@(Forall m xs [] _a)  (Forall n ys [] _b)
      | m /= n                 = fail "unifyType: forall: mismatched kind arity"
      | length xs /= length ys = fail "unifyType: forall: mismatched type arity"
      | otherwise = (t <$) . censor (const mempty) $ do
      ks <- lift $ replicateM m $ newSkolem ()
      let nxs = instantiate (pure . (ks!!)) <$> xs
          nys = instantiate (pure . (ks!!)) <$> ys
      _sks <- for (zip nxs nys) $ \(x,y) -> do
        k <- unifyKind mempty x y
        lift $ newSkolem k
      
      return ()
    go t@(HardType x) (HardType y) | x == y = return t
    -- go _ _ = fail "type mismatch"

unifyTV :: IntSet -> Int -> STRef s (Maybe (TypeM s)) -> STRef s Depth -> TypeM s -> ST s () -> WriterT Any (M s) (TypeM s)
unifyTV is i r d t bump = liftST (readSTRef r) >>= \ mt1 -> case mt1 of
  Just j | is^.ix i -> fail "type occurs"
         | otherwise -> do
    (t', Any m) <- listen $ unifyType (IntSet.insert i is) j t
    if m then liftST $ t' <$ writeSTRef r (Just t')
         else j <$ tell (Any True)
  Nothing -> case t of
    Var (Meta _ _ _ e _) -> do -- this has been semipruned so its not part of a chain
      tell (Any True)
      liftST $ do
        bump
        writeSTRef r (Just t)
        depth1 <- readSTRef d
        depth2 <- readSTRef e
        when (depth2 > depth1) $ writeSTRef e depth1
        return t
    _ -> do
      depth1 <- liftST $ readSTRef d
      if depth1 < maxBound
        then adjustDepth (IntSet.insert i is) depth1 t
        else return t -- we're done, zonk remaining cycles later

adjustDepth :: IntSet -> Depth -> TypeM s -> WriterT Any (M s) (TypeM s)
adjustDepth is depth1 = fmap join . traverse go where
  go v@(Meta _ j s d _)
    | is^.ix j  = fail "type occurs"
    | otherwise = liftST (readSTRef s) >>= \ mt -> case mt of
    Just t -> do -- we found a target
      tell (Any True) -- we're zonking
      t' <- adjustDepth (IntSet.insert j is) depth1 t -- chase children
      liftST $ t' <$ writeSTRef s (Just t')
    Nothing -> liftST $ do -- we didn't find a target, adjust the depth of this variable
      depth2 <- readSTRef d
      Var v <$ when (depth2 > depth1) (writeSTRef d depth1) -- boring
  go v = return (pure v)
