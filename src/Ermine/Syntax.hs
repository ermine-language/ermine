{-# LANGUAGE DefaultSignatures #-}
--------------------------------------------------------------------
-- |
-- Module    :  Ermine.Syntax
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
  , Tup'(..)
  , Tup(..)
  ) where

import Control.Lens

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
class Tup' t where
  tup :: [t] -> t

  default tup :: Tup t => [t] -> t
  tup = review tupled

-- | Discriminable tupling.
class Tup' t => Tup t where
  tupled :: Prism' t [t]

