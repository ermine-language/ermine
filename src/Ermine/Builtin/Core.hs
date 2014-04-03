{-# LANGUAGE TupleSections #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett and Dan Doel 2013-2014
-- License   :  BSD3
-- Maintainer:  Dan Doel <dan.doel@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Ermine.Builtin.Core
  ( plam
  -- * Host Literals
  , Lit(..)
  -- * Lists
  , cons
  , nil
  -- * Maybe
  , just
  , nothing
  ) where

import Bound
import Control.Applicative
import Control.Comonad
import Control.Lens.Cons
import qualified Data.HashMap.Lazy as HM
import Data.Int
import Data.Text hiding (zip, length, concatMap, cons)
import Data.Word
import Ermine.Builtin.Global
import Ermine.Builtin.Pattern
import Ermine.Pattern.Env
import Ermine.Pattern.Matrix
import Ermine.Pattern.Matching
import Ermine.Syntax.Convention
import Ermine.Syntax.Core
import Ermine.Syntax.Literal
import Ermine.Syntax.Pattern

plam :: (Eq v, MonadPattern m) => [P t v] -> Core v -> m (Core v)
plam ps body = Lam (C <$ ps) . Scope <$> compile ci pm
 where
 n = fromIntegral $ length ps :: Word8
 assocs = concatMap (\(i,Binder vs p) -> zip vs . fmap (ArgPP i) $ paths p) (zip [0..] ps)
 pm = PatternMatrix (pure . extract <$> ps)
              [Raw . Unguarded $ F . pure <$> abstract (`lookup` assocs) body]
 ci = Matching HM.empty (pure . B <$> [0..n-1]) (argPP <$> [0..n-1])

-- | The built-in '[]' constructor for a list.
nil :: Core a
nil = Data [] 0 nilg []

-- | The built-in 'Just' constructor for 'Maybe'.
just :: Core a -> Core a
just a = Data [C] 1 justg [a]

-- | The built-in 'Nothing' constructor for 'Maybe'.
nothing :: Core a
nothing = Data [] 0 nothingg []

-- | Lifting of literal values to core.
class Lit a where
  lit  :: a   -> Core b
  lits :: [a] -> Core b
  lits = Prelude.foldr (cons . lit) nil

instance Lit Int64 where
  lit l = Data [U] 0 literalg [HardCore $ Lit $ Long l]

instance Lit Int32 where
  lit i = Data [U] 0 literalg [HardCore $ Lit $ Int i]

instance Lit Char where
  lit c  = Data [U] 0 literalg [HardCore $ Lit $ Char c]
  lits = lit . pack

instance Lit Text where
  lit s = Data [N] 0 stringg [HardCore $ Lit $ String s]

instance Lit Int8 where
  lit b = Data [U] 0 literalg [HardCore $ Lit $ Byte b]

instance Lit Int16 where
  lit s = Data [U] 0 literalg [HardCore $ Lit $ Short s]

instance (Lit a, Lit b) => Lit (a, b) where
  lit (a,b) = Data [C,C] 0 (tupleg 2) [lit a, lit b]

instance Lit a => Lit [a] where
  lit = lits

instance Lit a => Lit (Maybe a) where
  lit = maybe nothing (just . lit)
