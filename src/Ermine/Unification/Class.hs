{-# LANGUAGE DeriveGeneric #-}

--------------------------------------------------------------------
-- |
-- Copyright :  (c) Dan Doel 2014
-- License   :  BSD2
-- Maintainer:  Dan Doel <dan.doel@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--------------------------------------------------------------------

module Ermine.Unification.Class
  ( ClassCheck(ClassCheck)
  , instantiateClass
  ) where

import Bound
import Bound.Scope
import Bound.Var
import Control.Applicative
import Control.Lens
import Data.Map as Map
import Data.Text
import Data.Traversable
import Data.Void
import Ermine.Syntax.Class
import Ermine.Syntax.Global
import Ermine.Syntax.Hint
import Ermine.Syntax.Kind as Kind
import Ermine.Syntax.Type as Type
import Ermine.Syntax.Term as Term
import Ermine.Unification.Meta
import GHC.Generics

data ClassCheck s = ClassCheck
                  { _cctparams :: [(Hint, KindM s)]
                  , _cccxt     :: [Scope Int (Type (KindM s)) Text]
                  , _ccsigs    :: Map Global (Type (KindM s) (Var Int Text))
                  , _ccdefs    :: Map Global (Bodies (Annot Void Text) Void)
                  }
  deriving (Eq, Show, Generic)

instantiateClass :: Class () Text -> M s (Schema (MetaK s), ClassCheck s)
instantiateClass cls = do
  clazz@(Class ks ts cxt sigs defs) <- kindVars (\_ -> newShallowMeta 0 False Nothing) cls
  mks <- for ks $ newMeta False
  tks <- for ts $ \(h, _) -> (,) h . pure <$> newShallowMeta 0 False Nothing
  return $ ( schema clazz
           , ClassCheck
               tks
               (hoistScope (over kindVars $ pure . unvar (mks!!) id) <$> cxt)
               (over kindVars (pure . unvar (mks!!) id) <$> sigs)
               defs
           )

