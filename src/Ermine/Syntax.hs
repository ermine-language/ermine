{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett 2012
-- License   :  BSD3
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
  , App (_App)
  , apps
  , ( # )
  -- * Fun
  , Fun(_Fun)
  , (~>)
  -- * Tup
  , Tup(_Tup)
  , tup
  ) where

import Bound
import Control.Lens
import Data.Tagged

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
