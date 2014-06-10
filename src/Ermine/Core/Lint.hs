{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Ermine.Core.Lint
  (
  -- * LintEnv
    LintEnv(..)
  , variables
  , foreignCxt
  , globalCxt
  -- * The Lint Monad
  , Lint(..)
  -- * Checking and inference
  , inferCore
  , checkCore
  , with
  ) where

import Bound
import Bound.Var
import Control.Applicative
import Control.Monad
import Control.Monad.Reader.Class
import Control.Lens
import Data.Data
import Data.Foldable (for_)
import Data.Hashable
import Data.Map as Map
import Ermine.Syntax.Convention
import Ermine.Syntax.Core
import Ermine.Syntax.Id
import Ermine.Syntax.Literal
import GHC.Generics

------------------------------------------------------------------------------
-- Form
------------------------------------------------------------------------------

data Form = Form [Convention] Convention
  deriving (Eq,Ord,Show,Read,Data,Typeable,Generic)

instance Hashable Form where
  hashWithSalt n (Form cc r) = n `hashWithSalt` cc `hashWithSalt` r

convention :: Form -> Convention
convention (Form [] r) = r
convention _           = C

------------------------------------------------------------------------------
-- LintEnv
------------------------------------------------------------------------------

data LintEnv a = LintEnv
  { _variables   :: a -> Either String Convention
  , _foreignCxt  :: Map Foreign Form
  , _globalCxt   :: Map Id Form
  } deriving Typeable

makeLenses ''LintEnv

instance Contravariant LintEnv where
  contramap f c = c { _variables = _variables c . f }

------------------------------------------------------------------------------
-- Lint
------------------------------------------------------------------------------

liftEither :: Monad m => Either String b -> m b
liftEither = either fail return

liftMaybe :: Monad m => String -> Maybe b -> m b
liftMaybe e = maybe (fail e) return

newtype Lint a b = Lint { runLint :: LintEnv a -> Either String b }

instance Bifunctor Lint where
  bimap f g = Lint . dimap (contramap f) (fmap g) . runLint

instance Functor (Lint a) where
  fmap f = Lint . fmap (fmap f) . runLint

instance Applicative (Lint a) where
  pure a = Lint $ \_ -> return a
  Lint mf <*> Lint ma = Lint $ \ c -> mf c <*> ma c
  Lint mf <*  Lint ma = Lint $ \ c -> mf c <*  ma c
  Lint mf  *> Lint ma = Lint $ \ c -> mf c  *> ma c

instance Alternative (Lint a) where
  empty = mzero
  (<|>) = mplus

instance Monad (Lint a) where
  return a = Lint $ \_ -> Right a
  fail s = Lint $ \_ -> Left s
  Lint m >>= f = Lint $ \c -> case m c of
    Left e -> Left e
    Right a -> runLint (f a) c

instance MonadPlus (Lint a) where
  mzero = Lint $ \_ -> Left "lint: failed"
  Lint ma `mplus` Lint mb = Lint $ \c -> case ma c of
    Right a -> Right a
    _       -> mb c

instance MonadReader (LintEnv a) (Lint a) where
  ask = Lint Right
  local f (Lint m) = Lint (m . f)

infix 0 `with`

with :: Lint b c -> (LintEnv a -> LintEnv b) -> Lint a c
with (Lint m) f = Lint (m . f)

------------------------------------------------------------------------------
-- Running Lint
------------------------------------------------------------------------------

inferHardCore :: HardCore -> Lint a Form
inferHardCore Super{}         = return $ Form [D] D
inferHardCore Slot{}          = return $ Form [D] C
inferHardCore (Lit String {}) = return $ Form [] N
inferHardCore Lit{}           = return $ Form [] U
inferHardCore Error{}         = return $ Form [] U
inferHardCore (Foreign p)     = preview (foreignCxt.ix p)  >>= liftMaybe "unknown foreign"
inferHardCore (Id h)          = preview (globalCxt.ix h)   >>= liftMaybe "unknown global"

checkCore :: Convention -> Core Convention a -> Lint a ()
checkCore cc c = do
  cc' <- convention <$> inferCore c
  when (cc' /= cc) $ fail $ "type mismatch: expected " ++ show cc ++ ", received " ++ show cc'

bindings :: Integral i => [Convention] -> LintEnv a -> LintEnv (Var i a)
bindings cs = variables %~ unvar (\i -> liftMaybe "illegal bound variable" $ cs^?ix (fromIntegral i))

binding :: Convention -> LintEnv a -> LintEnv (Var () a)
binding cc = variables %~ unvar (\_ -> Right cc)

inferCore :: Core Convention a -> Lint a Form
inferCore (Var a) = do
  v <- view variables
  Form [] <$> liftEither (v a)
inferCore (HardCore hc) = inferHardCore hc
inferCore (Lam cc body) = do
  r <- inferScope body `with` bindings cc
  when (convention r /= C) $ fail "bad lambda"
  return $ Form cc C
inferCore (App cc x y) = do
  checkCore cc y
  xs <- inferCore x
  case xs of
    f@(Form [] C)            -> return f -- unchecked application
    Form (c:cs) r | c == cc  -> return $ Form cs r
    _ -> fail "bad application"
inferCore (Data cc _ _ ps) = do
  when (length cc /= length ps) $ fail "bad data arguments"
  zipWithM_ checkCore cc ps
  return $ Form [] C
inferCore (Prim cc r _ ps) = do
  when (length cc /= length ps) $ fail "bad prim arguments"
  zipWithM_ checkCore cc ps
  return $ Form [] r
inferCore (Case b ms md) = do
  checkCore C b
  for_ ms $ \ (Match cc _ m) -> checkScope C m `with` bindings (C:cc)
  for_ md $ \ d -> checkScope C d `with` binding C
  return $ Form [] C
inferCore (Let bgs bdy) = do
  for_ bgs (checkScope C) `with` bindings (C <$ bgs)
  checkScope C bdy `with` bindings (C <$ bgs)
  return $ Form [] C
inferCore (Dict sups slts) = do
  for_ sups (checkCore D)
  for_ slts (checkScope C) `with` bindings (C <$ slts)
  return $ Form [] D

inferScope :: Scope b (Core Convention) a -> Lint (Var b a) Form
inferScope = inferCore . fromScope

checkScope :: Convention -> Scope b (Core Convention) a -> Lint (Var b a) ()
checkScope cc = checkCore cc . fromScope
