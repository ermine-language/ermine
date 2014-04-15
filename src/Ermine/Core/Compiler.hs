{-# LANGUAGE ViewPatterns #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett and Dan Doel 2014
-- License   :  BSD3
-- Maintainer:  Dan Doel <dan.doel@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------

module Ermine.Core.Compiler
  ( compile
  , compileBinding
  , compileBranches
  , compileHardCore
  ) where

import Bound
import Bound.Var
import Control.Lens
import Control.Monad.State
import Data.Foldable
import Data.Functor
import Data.List (elemIndex, nub)
import Data.Map hiding (null, filter, toList)
import qualified Data.Map as Map
import Data.Word
import Ermine.Syntax.G
import Ermine.Syntax.Convention as C
import Ermine.Syntax.Core (Core, HardCore, Match(..))
import qualified Ermine.Syntax.Core as Core
import Ermine.Syntax.Sort as S

data SortedRef = SortedRef Sort Ref
  deriving (Show)

sortedRefRef :: IndexedLens' Sort SortedRef Ref
sortedRefRef f (SortedRef s r) = SortedRef s r <$> indexed f s r

_SortedRef :: Sort -> Prism' SortedRef Ref
_SortedRef s = prism' (SortedRef s) $ \ xs -> case xs of
  SortedRef s' r | s == s' -> Right r
  _                        -> Left xs

c2s :: Convention -> Sort
c2s C.C = S.B
c2s C.D = S.B
c2s C.U = S.U
c2s C.N = S.N

genericLength :: Num n => [a] -> n
genericLength = fromIntegral . length

genericElemIndex :: (Eq a, Num n) => a -> [a] -> Maybe n
genericElemIndex x = fmap fromIntegral . elemIndex x

stackSorts :: [Sort] -> (Map Word32 SortedRef, Sorted Word32)
stackSorts xs = runState ?? 0 $ fmap fromList $ ifor xs $ \ i srt -> do
  sss <- sort srt <<+= 1
  return (fromIntegral i, SortedRef srt (Stack sss))

localSorts :: Eq v => [(v,SortedRef)] -> ([(v, SortedRef)], Sorted Word32)
localSorts xs cxt = runState ?? 0 $ for xs $ \(v,(srt,_)) -> do
  sss <- sort srt <<+= 1
  return (v, SortedRef srt (Local sss))

compileBinding :: Eq v => (v -> SortedRef) -> Core v -> PreClosure
compileBinding cxt co = PreClosure vs $ case co of
  Core.Lam [] _   -> error "PANIC: 0 arity core lambda"
  Core.Lam ccvs e ->
    noUpdate fvn args (compile (fvn + args) cxt'' $ fromScope e)
   where cxt'' (F v) = cxt' v & ifolded._Stack %@~ \s n -> n + args^.sort s
         cxt'' (B b) = m Map.! b
         (m, args) = stackSorts (c2s <$> ccvs)
  _ -> doUpdate fvn (compile fvn cxt' co)
 where
 vs = filter (hasn't $ _2.sortedRefRef._Global) . fmap (\v -> (v, cxt v)) . nub . toList $ co
 (fvs, fvn) = localSorts vs
 cxt' v = lookup v fvs <|> cxt v

compile :: Eq v => Sorted Word32 -> (v -> SortedRef) -> Core v -> G
compile n cxt (Core.Var v) = case cxt v of
  SortedRef B r -> App n (Ref r) []
  _             -> error "compile: Core.Var with unexpected variable convention"
compile _ _   (Core.HardCore hc) = compileHardCore hc
compile n cxt (Core.App cc f x)  = compileApp n cxt [(cc,x)] f
compile n cxt l@Core.Lam{} =
  Let [compileBinding cxt l] (App (n & sort B +~ 1) (Ref $ Local 0) [])
compile n cxt (Core.Case e bs d) =
  (if null cs then id else Let cs) $ Case e' $ compileBranches n' ev cxt' bs d
 where
 (cs, n', ev, e', cxt') = case e of
   Core.Var v                -> ([], n, cxt v, review _Ref $ view sortedRefRef $ cxt v, cxt)
   _ | n' <- n & sort B +~ 1 -> ([compileBinding cxt e], n', SortedRef B (Local 0), _Ref n'._Local # 0, cxt'')
     where cxt'' = cxt & mapped._SortedRef B._Stack +~ 1
compile n cxt (Core.Let bs e) =
  -- TODO: check for acyclicity
  LetRec bs' . compile (n & sort B +~ l) cxt' $ fromScope e
 where
 l = genericLength bs
 cxt' (F v) = cxt v & _SortedRef B._Local +~ l
 cxt' (B b) = _SortedRef B . Local # b
 bs' = compileBinding cxt' . fromScope <$> bs
compile _ _   (Core.Data _    _ _ _ ) = error "compile: Data"
compile _ _   (Core.Dict _ _) = error "compile: Dict"
compile _ _   (Core.CaseLit _ _ _ _) = error "compile: CaseLit"

-- TODO: handle calling conventions
compileBranches
  :: Eq v
  => Sorted Word32
  -> SortedRef
  -> (v -> SortedRef)
  -> Map Word8 (Match Core v)
  -> Maybe (Scope () Core v)
  -> Continuation
compileBranches n ev cxt bs d = Continuation bs' d'
 where
 bs' = bs <&> \(Match ccvs _ e) ->
   let cxt' = unvar bc $ cxt & mapped.sortedRefRef._Stack %@~ \s r -> r + fields^.sort s
       (m, fields) = stackSort (c2s <$> ccvs)
       bc 0 = ev & sortedRefRef._Stack +~ fields^.sort B
       bc b = m Map.! (b - 1)
    in (fields, compile (n+fields) cxt' (fromScope e))
 d' = compile n (unvar (const ev) cxt) . fromScope <$> d

anf :: Eq v
    => Sorted Word32 -> (v -> SortedRef)
    -> LensLike (State (Word32, [PreClosure])) s t (Core v) Ref
    -> s -> (t, (Word32, [PreClosure]))
anf n cxt ll s = runState (ll compilePiece s) (n, [])
 where
 compilePiece (Core.Var v) = return $ cxt v
 compilePiece co = state $ \(k,l) ->
   let bnd = compileBinding cxt co
    in (Local k,(k+1,bnd:l))

compileApp :: Eq v => Word32 -> (v -> Ref) -> [Core v] -> Core v -> G
compileApp n cxt xs (Core.App _ f x) = compileApp n cxt (x:xs) f
compileApp n cxt xs f = (if null bs then id else Let bs) $ App (Ref f') xs'
 where
 ((f', xs'), (_, bs)) = anf n cxt (beside id traverse) (f, xs)

compileHardCore :: HardCore -> G
compileHardCore _ = undefined
