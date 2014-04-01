{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}

--------------------------------------------------------------------
-- |
-- Copyright :  (c) Dan Doel and Edward Kmett 2013-2014
-- License   :  BSD3
-- Maintainer:  Dan Doel <dan.doel@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Ermine.Pattern.Env
  ( PatternEnv(..)
  , dummyPatternEnv
  , MonadPattern(..)
  , isSignature
  , constructorTag
  , constructorGlobal
  ) where

import Prelude hiding (all)

import Control.Applicative
import Control.Lens
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HM
import Data.Set (Set)
import qualified Data.Set as S
import Data.Word
import Ermine.Syntax.Global
import Ermine.Syntax.Pattern

#ifdef HLINT
{-# ANN module "hlint: ignore Use fromMaybe" #-}
#endif

-- | The environment necessary to perform pattern compilation. We need two
-- pieces of information:
--
--   1) The signatures (full list of constructors) associated with each
--      particular constructor
--
--   2) A mapping from constructors to their @Core@ integer tag
--
-- This is accomplished via a map of maps. The outer map should take each
-- global to its associated signature, and signatures are represented as
-- maps from globals to integer tags.
newtype PatternEnv = PatternEnv { signatures :: HashMap Global (HashMap Global Word8) }
  deriving (Eq, Show)

dummyPatternEnv :: PatternEnv
dummyPatternEnv = PatternEnv $
  HM.fromList [(eg, esig), (justg, maybesig), (nothingg, maybesig)]
 where
  esig = HM.singleton eg 0
  maybesig = HM.fromList [(nothingg, 0), (justg, 1)]

-- | Monads that allow us to perform pattern compilation, by providing
-- a PatternEnv.
class (Applicative m, Monad m) => MonadPattern m where
  askPattern :: m PatternEnv

instance MonadPattern ((->) PatternEnv) where
  askPattern = id

-- | Determines whether a set of pattern heads constitutes a signature.
-- This is handled specially for tuples and literals, and relies on the
-- monad for data type constructors.
isSignature :: MonadPattern m => Set PatternHead -> m Bool
isSignature ps = case preview folded ps of
  Nothing         -> pure False
  Just (LitH _)   -> pure False -- too big, assume no
  Just (TupH _)   -> pure True
  Just (ConH _ _ g) -> askPattern <&> \env -> case HM.lookup g $ signatures env of
    Nothing -> error "PANIC: isSignature: unknown constructor"
    Just hm -> iall (\g' _ -> S.member g' ns) hm
 where ns = S.map _name ps

-- | Looks up the constructor tag for a pattern head. For tuples this is
-- always 0, but constructors must consult the compilation environment.
constructorTag :: MonadPattern m => PatternHead -> m Word8
constructorTag (LitH _) = error "PANIC: constructorTag: literal head"
constructorTag (TupH _) = pure 0
constructorTag (ConH _ _ g) = askPattern <&> \env ->
  case HM.lookup g (signatures env) >>= HM.lookup g of
    Nothing -> error "PANIC: constructorTag: unknown constructor"
    Just i  -> i

constructorGlobal :: PatternHead -> Global
constructorGlobal (TupH n)     = tupleg n
constructorGlobal (ConH _ _ g) = g
constructorGlobal LitH{}       = error "PANIC: constructorGlobal: literal head"
