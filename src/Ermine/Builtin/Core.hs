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
  ) where

import Bound
import Control.Applicative
import Control.Comonad
import qualified Data.HashMap.Lazy as HM
import Data.Word
import Ermine.Builtin.Pattern
import Ermine.Pattern.Env
import Ermine.Pattern.Matrix
import Ermine.Pattern.Matching
import Ermine.Syntax.Convention
import Ermine.Syntax.Core
import Ermine.Syntax.Pattern

plam :: (Eq v, MonadPattern m) => [P t v] -> Core v -> m (Core v)
plam ps body = Lam (C <$ ps) C . Scope <$> compile ci pm
 where
 n = fromIntegral $ length ps :: Word8
 assocs = concatMap (\(i,Binder vs p) -> zip vs . map (ArgPP i) $ paths p) (zip [0..] ps)
 pm = PatternMatrix (map (pure . extract) ps)
              [Raw . Unguarded $ F . pure <$> abstract (`lookup` assocs) body]
 ci = Matching HM.empty (map (pure . B) [0..n-1]) (map argPP [0..n-1])
