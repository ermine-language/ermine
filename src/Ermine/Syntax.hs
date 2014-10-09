{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett 2012
-- License   :  BSD2
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Ermine.Syntax
  (
  -- * Variables
    Variable(_Var)
  -- * Application
  , App(_App)
  , apps
  , ( ## )
  -- * Strict Unboxed Application
  , AppHash(_AppHash)
  , appHashes
  -- * Dictionary Application
  , AppDict(_AppDict)
  , appDicts
  -- * Fun
  , Fun(_Fun)
  , (~>)
  -- * Tup
  , Tup(_Tup)
  , tup
  , tup'
  -- * Utilities
  , memoverse
  , bimemoverse
  ) where

import Bound
import Bound.Var
import Control.Lens
import Control.Applicative
import qualified Data.Map as Map
import Control.Monad.State
import Data.Bitraversable
import Data.Tagged
import Ermine.Syntax.Scope

--------------------------------------------------------------------
-- Variable
--------------------------------------------------------------------

-- | Discriminable 'pure'.
class Variable t where
  -- | If @t@ is also 'Applicative', then it should satisfy these laws:
  --
  -- @
  -- 'pure' a '^?' '_Var' ≡ 'Just' a
  -- 'pure' ≡ 'review' '_Var'
  -- @
  _Var :: Prism' (t a) a

instance Variable Maybe where
  _Var = _Just

instance Variable [] where
  _Var = prism return $ \xs -> case xs of
    [x] -> Right x
    _   -> Left xs

instance Variable (Either b) where
  _Var = _Right

instance Variable m => Variable (Scope b m) where
  _Var = _Scope._Var._F._Var

--------------------------------------------------------------------
-- App
--------------------------------------------------------------------

-- | Discriminable 'App'
--
-- This prism provides ad hoc overloading of construction and pattern
-- matching on 'App'.
class App t where
  _App :: Prism' (t a) (t a, t a)

instance (Variable t, App t) => App (Scope b t) where
  _App = prism (\ (Scope x, Scope y) -> Scope $ x ## y) $ \t@(Scope b) -> case b^?_App of
    Just (x,y) -> Right (Scope x, Scope y)
    _ -> case b^?_Var of
      Just (F xy) -> case xy^?_App of
        Just (x,y) -> Right (Scope (_Var # F x), Scope (_Var # F y))
        _ -> Left t
      _ -> Left t

infixl 9 ##

-- | Convenient infix application operator.
(##) :: App t => t a -> t a -> t a
(##) = curry (review _App)

-- | Fold a series of applications.
apps :: App t => t a -> [t a] -> t a
apps = foldl (##)

--------------------------------------------------------------------
-- AppHash
--------------------------------------------------------------------

class AppHash t where
  _AppHash :: Prism' (t a) (t a, t a)

instance (Variable t, AppHash t) => AppHash (Scope b t) where
  _AppHash = prism (\ (Scope x, Scope y) -> Scope $ _AppHash # (x,y)) $ \t@(Scope b) -> case b^?_AppHash of
    Just (x,y) -> Right (Scope x, Scope y)
    _ -> case b^?_Var of
      Just (F xy) -> case xy^?_AppHash of
        Just (x,y) -> Right (Scope (_Var # F x), Scope (_Var # F y))
        _ -> Left t
      _ -> Left t

-- | Fold a series of unboxed applications.
appHashes :: AppHash t => t a -> [t a] -> t a
appHashes = foldl $ curry $ review _AppHash

--------------------------------------------------------------------
-- AppDict
--------------------------------------------------------------------

-- | 'AppDict' provides strict application for dictionary manipulation
class AppDict c where
  _AppDict :: Prism' (c a) (c a, c a)

instance (Variable t, AppDict t) => AppDict (Scope b t) where
  _AppDict = prism (\ (Scope x, Scope y) -> Scope $ _AppDict # (x,y)) $ \t@(Scope b) -> case b^?_AppDict of
    Just (x,y) -> Right (Scope x, Scope y)
    _ -> case b^?_Var of
      Just (F xy) -> case xy^?_AppDict of
        Just (x,y) -> Right (Scope (_Var # F x), Scope (_Var # F y))
        _ -> Left t
      _ -> Left t

-- | Apply several dictionaries.
appDicts :: AppDict t => t a -> [t a] -> t a
appDicts = Prelude.foldl $ curry $ review _AppDict

--------------------------------------------------------------------
-- Fun
--------------------------------------------------------------------

-- | Discriminable syntactic arrows.
class Fun t where
  _Fun :: Prism' (t a) (t a, t a)

infixr 0 ~>

-- | Provide ad hoc overloading of function arrows.
(~>) :: Fun t => t a -> t a -> t a
(~>) = curry (review _Fun)

--------------------------------------------------------------------
-- Tup
--------------------------------------------------------------------

-- | (Possibly) Discriminable tupling.
class (Reviewable p, Functor f) => Tup p f t where
  _Tup :: Optic' p f t [t]

tup :: Tup Tagged Identity t => [t] -> t
tup = review _Tup

tup' :: Tup Tagged Identity t => [t] -> t
tup' [x] = x
tup' xs = review _Tup xs

------------------------------------------------------------------------------
-- Utilities
------------------------------------------------------------------------------

-- | A version of bitraverse that only runs the functions in question once for
-- each distinct value in the traversable container, remembering the result on
-- the first run, and reusing it.
bimemoverse :: (Bitraversable t, Applicative f, Monad f, Ord k, Ord a)
            => (k -> f l) -> (a -> f b)
            -> t k a -> f (t l b)
bimemoverse knd typ =
  flip evalStateT (Map.empty, Map.empty) . bitraverse (memoed _1 knd) (memoed _2 typ)

memoverse :: (Traversable t, Applicative f, Monad f, Ord a) => (a -> f b) -> t a -> f (t b)
memoverse typ = flip evalStateT Map.empty . traverse (memoed id typ)

memoed :: (Ord v, Monad f) => Lens' s (Map.Map v u) -> (v -> f u) -> v -> StateT s f u
memoed l g v = use l >>= \m -> case m ^. at v of
 Just v' -> return v'
 Nothing -> do v' <- lift (g v)
               l.at v ?= v'
               return v'

