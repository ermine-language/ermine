module Ermine.App
  ( App (..)
  , apps
  , (#)
  ) where

import Control.Lens

class App t where
  app :: Prism' t (t,t)

infixl 9 #

(#) :: App t => t -> t -> t
(#) = curry (review app)

apps :: App t => t -> [t] -> t
apps = foldl (#)
