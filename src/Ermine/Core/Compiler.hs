
--------------------------------------------------------------------
-- |
-- Copyright :  (c) Dan Doel 2014
-- License   :  BSD3
-- Maintainer:  Dan Doel <dan.doel@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------

module Ermine.Core.Compiler where

{-

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
import Data.Word
import Ermine.Syntax.G
import Ermine.Syntax.Core (Core, HardCore, Match(..))
import qualified Ermine.Syntax.Core as Core

genericLength :: Num n => [a] -> n
genericLength = fromIntegral . length

genericElemIndex :: (Eq a, Num n) => a -> [a] -> Maybe n
genericElemIndex x = fmap fromIntegral . elemIndex x

compileBinding :: Eq v => (v -> Ref) -> Core v -> PreClosure
compileBinding cxt co = (,) vs $ case co of
  -- Assumes that core lambdas are never 0-arity
  Core.Lam ccvs e ->
    noUpdate fvn bvn (compile (fvn + fromIntegral bvn) cxt'' $ fromScope e)
   where bvn  = genericLength ccvs
         cxt'' (F v) = cxt' v
         cxt'' (B b) = Local $ fvn + fromIntegral b
  -- TODO: this is naive
  _ -> doUpdate fvn (compile fvn cxt' $ co)
 where
 (fvs, vs) =
   unzip . filter (has $ _2._Local) . fmap (\v -> (v, cxt v)) . nub . toList $ co
 fvn = genericLength vs

 cxt' v | Just n <- genericElemIndex v fvs = Local n
        | otherwise                        = cxt v

compile :: Eq v => Word32 -> (v -> Ref) -> Core v -> G
compile _ cxt (Core.Var v) = App (Ref $ cxt v) []
compile _ _   (Core.HardCore hc) = compileHardCore hc
compile n cxt (Core.App _   f x) = compileApp n cxt [x] f
compile n cxt l@Core.Lam{} =
  Let [compileBinding cxt l] (App (Ref $ Local n) [])
compile n cxt (Core.Case e bs d) =
  (if null cs then id else Let cs) . Case e' $ compileBranches n' ev cxt bs d
 where
 (cs, n', ev, e') = case e of
   Core.Var v -> ([], n, cxt v, _Ref # cxt v)
   _          -> ([compileBinding cxt e], n+1, Local n, review _Ref . Local $ n)
compile n cxt (Core.Let bs e) =
  Let bs' . compile (n + genericLength bs) cxt' $ fromScope e
 where
 cxt' (F v) = cxt v
 cxt' (B b) = Local $ n + b
 bs' = compileBinding cxt' . fromScope <$> bs
compile _ _   (Core.Data _    _ _ _ ) = error "compile: Data"
compile _ _   (Core.Dict _ _) = error "compile: Dict"
compile _ _   (Core.CaseLit _ _ _ _) = error "compile: CaseLit"

-- TODO: handle calling conventions
compileBranches
  :: Eq v
  => Word32
  -> Ref
  -> (v -> Ref)
  -> Map Word8 (Match Core v)
  -> Maybe (Scope () Core v)
  -> Continuation
compileBranches n ev cxt bs d = Continuation bs' d'
 where
 bs' = bs <&> \(Match ccvs _ e) ->
   let cxt' = unvar bc cxt
       bc 0 = ev
       bc b = Local . (n-1+) . fromIntegral $ b
    in (genericLength ccvs, compile (n+genericLength ccvs) cxt' (fromScope e))
 d' = compile n (unvar (const ev) cxt) . fromScope <$> d

anf :: Eq v
    => Word32 -> (v -> Ref)
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

-}
