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
    Variable(..)
  -- * Application
  , App (..)
  , apps
  , ( # )
  -- * Fun
  , Fun(..)
  , (~>)
  -- * Tup
  , Tup(..)
  , tup
  ) where

import Bound
import Control.Lens
import Control.Lens.Internal.Review
import Data.Functor.Identity

--------------------------------------------------------------------
-- Variable
--------------------------------------------------------------------

-- | Discriminable 'pure'.
class Variable t where
  -- | If @t@ is also 'Applicative', then it should satisfy these laws:
  --
  -- @
  -- 'pure' a '^?' 'var' ≡ 'Just' a
  -- 'pure' ≡ 'review' 'var'
  -- @
  var :: Prism' (t a) a

instance Variable Maybe where
  var = _Just

instance Variable [] where
  var = prism return $ \xs -> case xs of
    [x] -> Right x
    _   -> Left xs

instance Variable (Either b) where
  var = _Right

--------------------------------------------------------------------
-- App
--------------------------------------------------------------------

-- | Discriminable 'App'
--
-- This prism provides ad hoc overloading of construction and pattern
-- matching on 'App'.
class App t where
  app :: Prism' t (t, t)

instance (Variable t, App (t (Var b (t a))), App (t a)) => App (Scope b t a) where
  app = prism (\ (Scope x, Scope y) -> Scope $ x ## y) $ \t@(Scope b) -> case b^?app of
    Just (x,y) -> Right (Scope x, Scope y)
    _ -> case b^?var of
      Just (F xy) -> case xy^?app of
        Just (x,y) -> Right (Scope (var # F x), Scope (var # F y))
        _ -> Left t
      _ -> Left t

infixl 9 ##

-- | Convenient infix application operator.
(##) :: App t => t -> t -> t
(##) = curry (review app)

-- | Fold a series of applications.
apps :: App t => t -> [t] -> t
apps = foldl (##)

--------------------------------------------------------------------
-- Fun
--------------------------------------------------------------------

-- | Discriminable syntactic arrows.
class Fun t where
  fun :: Prism' t (t, t)

infixr 0 ~>

-- | Provide ad hoc overloading of function arrows.
(~>) :: Fun t => t -> t -> t
(~>) = curry (review fun)

--------------------------------------------------------------------
-- Tup
--------------------------------------------------------------------

-- | Irreversible tupling.
{-
class Tup' t where

  default tup :: Tup t => [t] -> t
  tup = review tupled
-}

-- | (Possibly) Discriminable tupling.
class (Reviewable p, Functor f) => Tup p f t where
  tupled :: Overloaded' p f t [t]

tup :: Tup Reviewed Identity t => [t] -> t
tup = review tupled
