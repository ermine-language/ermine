{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
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
  ( SortRef(..)
  , compile
  , compileBinding
  , compileBranches
  , compileHardCore
  ) where

import Bound
import Bound.Var as Var
import Control.Applicative
import Control.Lens
import Control.Monad.State
import Data.Foldable as F
import Data.List (nub)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Map hiding (null, filter, toList)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Monoid
import Data.Traversable
import Data.Vector as Vector (Vector, fromList, length, generate, singleton)
import Data.Word
import Ermine.Syntax.G
import Ermine.Syntax.Convention as C
import Ermine.Syntax.Core (Core, HardCore, Match(..))
import Ermine.Syntax.Literal
import qualified Ermine.Syntax.Core as Core
import Ermine.Syntax.Sort as S

data SortRef = SortRef Sort Ref
  deriving Show

sortRef :: IndexedLens' Sort SortRef Ref
sortRef f (SortRef s r) = SortRef s <$> indexed f s r

_SortRef :: Sort -> Prism' SortRef Ref
_SortRef s = prism (SortRef s) $ \ xs -> case xs of
  SortRef s' r | s == s' -> Right r
  _                      -> Left xs

c2s :: Convention -> Sort
c2s C.C = S.B
c2s C.D = S.B
c2s C.U = S.U
c2s C.N = S.N

sortRefs :: [SortRef] -> Sorted (Vector Ref)
sortRefs = fmap Vector.fromList
         . Prelude.foldr (\(SortRef s r) -> sort s %~ (r:)) mempty

genericLength :: Num n => [a] -> n
genericLength = fromIntegral . Prelude.length

stackSorts :: [Sort] -> (Map Word64 SortRef, Sorted Word64)
stackSorts xs = runState ?? 0 $ fmap Map.fromList $ ifor xs $ \ i srt -> do
  sss <- sort srt <<+= 1
  return (fromIntegral i, SortRef srt (Stack sss))

localSorts :: Eq v => [(v,SortRef)] -> ([(v, SortRef)], Sorted Word64)
localSorts xs = runState ?? 0 $ for xs $ \(v,SortRef srt _) -> do
  sss <- sort srt <<+= 1
  return (v, SortRef srt (Local sss))

compileBinding :: Eq v => (v -> SortRef) -> Core Convention v -> PreClosure
compileBinding cxt co = case co of
  Core.Lam [] _   -> error "PANIC: 0 arity core lambda"
  Core.Lam ccvs e ->
    PreClosure (sortRefs $ snd <$> vs) $
      noUpdate fvn args (compile args cxt'' $ fromScope e)
   where cxt'' (Var.F v) = cxt' v & sortRef._Stack %@~ \s n -> n + args^.sort s
         cxt'' (Var.B b) = m Map.! b
         (m, args) = stackSorts (c2s <$> ccvs)
  Core.Data [C.U] t _ [Core.HardCore (Core.Lit l)] | Just w <- literalRep l ->
    PreClosure (Sorted mempty (Vector.singleton (Lit w)) mempty)
               (standardConstructor (Sorted 0 1 0) t)
  Core.Data [C.U] t _ [Core.Var v] | SortRef _ r <- cxt v ->
    PreClosure (Sorted mempty (Vector.singleton r) mempty)
               (standardConstructor (Sorted 0 1 0) t)
  _ -> PreClosure (sortRefs $ snd <$> vs) $ doUpdate fvn (compile 0 cxt' co)
 where
 vs = filter (hasn't $ _2.sortRef.(_Global.united<>_Lit.united))
    . fmap (\v -> (v, cxt v)) . nub . toList $ co
 (fvs, fvn) = localSorts vs
 cxt' v = fromMaybe (cxt v) $ Prelude.lookup v fvs

let_ :: [PreClosure] -> G -> G
let_ [] g = g
let_ xs g = Let (Vector.fromList xs) g

letRec :: [PreClosure] -> G -> G
letRec [] g = g
letRec xs g = LetRec (Vector.fromList xs) g

compile :: Eq v => Sorted Word64 -> (v -> SortRef) -> Core Convention v -> G
compile n cxt (Core.Var v) = case cxt v of
  SortRef S.B r -> _Ref n # r
  _             -> error "compile: Core.Var with unexpected variable convention"
compile n _ (Core.HardCore (Core.Slot i)) = let_ [PreClosure mempty $ LambdaForm 0 (Sorted 1 0 0) False $ App (Sorted 1 0 0) (Ref $ Stack 0) $ Sorted mempty (Vector.singleton (Lit i)) mempty] $ _Ref (n & sort S.B +~ 1) # Stack 0
compile n _ (Core.HardCore (Core.Super i)) = let_ [PreClosure mempty $ LambdaForm 0 (Sorted 1 0 0) False $ App (Sorted 1 0 0) (Ref $ Stack 0) $ Sorted mempty (Vector.singleton (Lit i)) mempty] $ _Ref (n & sort S.B +~ 1) # Stack 0
compile n _ (Core.HardCore (Core.Id i)) = App n (Ref (Global i)) mempty
compile _ _ (Core.HardCore hc) = compileHardCore hc

compile n cxt (Core.App cc f x)  = compileApp n cxt [(cc,x)] f
compile n cxt l@Core.Lam{} =
  let_ [compileBinding cxt l] (App (n & sort S.B +~ 1) (Ref $ Stack 0) mempty)
compile n cxt (Core.Case e bs d) = case e of
  Core.Var v ->
    Case (_Ref 0 # view sortRef (cxt v)) $ compileBranches n (cxt v) cxt bs d
  _          ->
     let_ [compileBinding cxt e]
     $ Case (_Ref 0 # Stack 0)
       $ compileBranches n' (SortRef S.B (Stack 0)) cxt' bs d
    where n'   = n & sort S.B +~ 1
          cxt' = cxt & mapped._SortRef S.B ._Stack +~ 1
compile n cxt (Core.Let bs e) =
  letRec bs' . compile (n & sort S.B +~ l) cxt' $ fromScope e
 where
  l = genericLength bs
  cxt' (Var.F v) = cxt v & _SortRef S.B ._Stack +~ l
  cxt' (Var.B b) = _SortRef S.B . _Stack # b
  bs' = compileBinding cxt' . fromScope <$> bs
compile n cxt (Core.Data ccvs tag _ xs) = case anf cxt (zip ccvs xs) of
    (refs, k, pcs) ->
        let_ (pcs ++ [PreClosure srefs con])
        $ App (n & sort S.B +~ k + 1) (Ref $ Stack k) mempty
      where srefs = sortRefs refs
            con = standardConstructor (fromIntegral.Vector.length <$> srefs) tag
compile n cxt (Core.Dict sups slts)   =
  letRec (compileBinding cxt' . fromScope <$> slts) $
  let_ (compileBinding (cxt'.F) <$> sups) $
  let_ [PreClosure caps (dictionary $ fromIntegral k)] $
    _Ref (n & sort S.B +~ fromIntegral k + 1) # Stack 0
  where
    kslts = Prelude.length slts
    k = Prelude.length sups + kslts
    caps = Sorted (Vector.generate k $ Stack . fromIntegral) mempty mempty
    cxt' (Var.F v) = cxt v & _SortRef S.B ._Stack +~ fromIntegral kslts
    cxt' (Var.B b) = _SortRef S.B . _Stack # b
compile _ _   (Core.Prim _ _ _ _)     = error "compile: Prim"

-- TODO: handle calling conventions
compileBranches
  :: Eq v
  => Sorted Word64
  -> SortRef
  -> (v -> SortRef)
  -> Map Word64 (Match Convention (Core Convention) v)
  -> Maybe (Scope () (Core Convention) v)
  -> Continuation
compileBranches n ev cxt bs d = Continuation bs' d'
 where
 bs' = bs <&> \(Match ccvs _ e) ->
   let cxt' = unvar bc $ cxt & mapped.sortRef._Stack %@~ \s r -> r + fields^.sort s
       (m, fields) = stackSorts (c2s <$> ccvs)
       bc 0 = ev & sortRef._Stack +~ fields^.sort S.B
       bc b = m Map.! fromIntegral (b - 1)
    in (fields, compile (n+fields) cxt' (fromScope e))
 d' = compile n (unvar (const ev) cxt) . fromScope <$> d

-- TODO: literal handling
anf :: (Traversable t, Eq v)
    => (v -> SortRef)
    -> t (Convention, Core Convention v)
    -> (t SortRef, Word64, [PreClosure])
anf cxt s = cleanup $ runState (traverse (uncurry compilePiece) s) (0, []) where
  cleanup (nebs,(n,pcs)) =
    (nebs <&> itraversed.indices id._SortRef S.B ._Stack +~ n <&> snd, n, reverse pcs)

  compilePiece _ (Core.Var v) = return (True, cxt v)
  compilePiece cv (Core.HardCore (Core.Id i))
    | cv `F.elem` [C.C, C.D] = return (False, SortRef S.B $ Global i)
  compilePiece C.N (Core.HardCore (Core.Lit (String str))) =
    return (False, SortRef S.N $ _UnsafeNative # str)
  compilePiece C.N (Core.HardCore (Core.Lit (Integer i))) =
    return (False, SortRef S.N $ _UnsafeNative # i)
  compilePiece C.U (Core.HardCore (Core.Lit l)) = case literalRep l of
      Just r -> return (False, SortRef S.U (Lit r))
      _      -> error "anf: exotic literal"
  compilePiece cv co | cv `F.elem` [C.C, C.D] = state $ \(k,l) ->
    let bnd = compileBinding cxt co
     in ((False,SortRef S.B $ Stack k),(k+1,bnd:l))
  compilePiece _ _ = error "anf: TODO"

compileApp :: Eq v => Sorted Word64 -> (v -> SortRef) -> [(Convention, Core Convention v)] -> Core Convention v -> G
compileApp n cxt xs (Core.App cc f x) = compileApp n cxt ((cc,x):xs) f
compileApp n cxt xs f = case anf cxt ((C.C, f) :| xs) of
  (SortRef S.B f' :| xs', k, bs) ->
    let_ bs $ App (n & sort S.B +~ k) (Ref f') (sortRefs xs')
  _ -> error "compileApp: unexpected sort"

compileHardCore :: HardCore -> G
compileHardCore hc = error $ "TODO: compileHardCore: " ++ show hc

-- physical storage of literals
literalRep :: Literal -> Maybe Word64
literalRep (Int i)   = Just $ fromIntegral i
literalRep (Long i)  = Just $ fromIntegral i
literalRep (Byte i)  = Just $ fromIntegral i
literalRep (Short i) = Just $ fromIntegral i
literalRep Float{}   = Nothing
literalRep Double{}  = Nothing
literalRep String{}  = Nothing
literalRep Integer{} = Nothing
literalRep (Char c)  = Just $ fromIntegral (fromEnum c)
