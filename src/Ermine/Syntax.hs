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
  , (#)
  -- * Fun
  , Fun(..)
  , (~>)
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
  var = _just

instance Variable [] where
  var = prism return $ \xs -> case xs of
    [x] -> Right x
    _   -> Left xs

instance Variable (Either b) where
  var = _right

--------------------------------------------------------------------
-- App
--------------------------------------------------------------------

-- | Discriminable 'App'
--
-- This prism provides ad hoc overloading of construction and pattern
-- matching on 'App'.
class App t where
  app :: Prism' (t a) (t a, t a)

infixl 9 #

-- | Convenient infix application operator.
(#) :: App t => t a -> t a -> t a
(#) = curry (review app)

-- | Fold a series of applications.
apps :: App t => t a -> [t a] -> t a
apps = foldl (#)

--------------------------------------------------------------------
-- Fun
--------------------------------------------------------------------

-- | Discriminable syntactic arrows.
class Fun t where
  fun :: Prism' (t a) (t a,t a)

infixr 0 ~>

-- | Provide ad hoc overloading of function arrows.
(~>) :: Fun t => t a -> t a -> t a
(~>) = curry (review fun)
