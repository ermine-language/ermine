module Ermine.Fun
  ( Fun(..)
  , (~>)
  ) where

import Control.Lens

class Fun t where
  fun :: Prism' t (t,t)

infixr 0 ~>

(~>) :: Fun t => t -> t -> t
(~>) = curry (review fun)
