{-# LANGUAGE TupleSections #-}

--------------------------------------------------------------------
-- |
-- Copyright :  (c) Dan Doel 2013
-- License   :  BSD3
-- Maintainer:  Dan Doel <dan.doel@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------

module Ermine.Builtin.Core
  ( plam
  , plamBranch
  ) where

import Bound
import Control.Applicative
import Control.Lens
import Control.Comonad
import Data.List (transpose)
import qualified Data.HashMap.Lazy as HM
import Data.Text as SText (pack)
import Data.Word
import Ermine.Builtin.Pattern
import Ermine.Syntax.Core
import Ermine.Syntax.Pattern
import Ermine.Syntax.Pattern.Compiler


plam :: (Eq v, MonadPComp m) => [P t v] -> Core v -> m (Core v)
plam ps body = Lam n . Scope <$> compile ci pm
 where
 n = fromIntegral $ length ps :: Word8
 assocs = concatMap (\(i,(Binder vs p)) -> zip vs . map (ArgPP i) $ paths p) (zip [0..] ps)
 pm = PMatrix (map (pure . extract) ps)
              [Trivial]
              [F . pure <$> abstract (`lookup` assocs) body]
 ci = CInfo HM.empty (map (pure . B) [0..n-1]) (map argPP [0..n-1])

plamBranch :: (Eq v, MonadPComp m) => [([P t v], [(Maybe (Core v), Core v)])] -> m (Core v)
plamBranch bs | null bs = pure . HardCore $ Error $ SText.pack "Empty lambda branch"
              | any ((/= n) . fromIntegral . length) ps =
                  pure . HardCore $ Error $ SText.pack "Non-uniform patterns"
              | otherwise = Lam n . Scope <$> compile ci pm
 where (bps, gs, cs) = unzip3 $ bs >>= \(bp, gcs) -> map (uncurry (bp,,)) gcs
       n  = fromIntegral . length $ head ps :: Word8
       ps = map (map extract) bps
       is = map (concatMap (\(i,(Binder vs p)) -> zip vs . map (ArgPP i) $ paths p)
                 . zip [0..]) bps
       mkguard _ Nothing  = Trivial
       mkguard i (Just c) = Explicit $ F . pure <$> abstract (`lookup` i) c
       pm = PMatrix (transpose ps) (zipWith mkguard is gs)
                    (zipWith (\i c -> F . pure <$> abstract (`lookup` i) c) is cs)
       ci = CInfo HM.empty (map (pure . B) [0..n-1]) (map argPP [0..n-1])
