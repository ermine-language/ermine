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
-- import Data.List (transpose)
import qualified Data.HashMap.Lazy as HM
-- import Data.Text as SText (pack)
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
              [Raw . Unguarded $ F . pure <$> abstract (`lookup` assocs) body]
 ci = CInfo HM.empty (map (pure . B) [0..n-1]) (map argPP [0..n-1])
