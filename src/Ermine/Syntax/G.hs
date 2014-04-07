{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) Dan Doel 2014
-- License   :  BSD3
-- Maintainer:  Dan Doel <dan.doel@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Ermine.Syntax.G
  ( Ref(..)
  , _Global
  , _Local
  , HasRef(..)
  , G(..)
  , _Ref
  , PreClosure
  , Tag
  , Continuation(..)
  , Func(..)
  , LambdaForm(LambdaForm)
  , free
  , bound
  , update
  , body
  , noUpdate
  , doUpdate
  , standardConstructor
  ) where

import Control.Lens
import Data.Functor
import Data.Word
import Data.Map hiding (update)
import Ermine.Syntax.Convention

data Ref
  = Global { _refConvention :: !Convention, _refId :: !Word32 }
  | Local  { _refConvention :: !Convention, _refId :: !Word32 }
  deriving Show

makePrisms ''Ref
makeClassy ''Ref

data G
  = Case !G !Continuation
  | App !Func [Ref]
  | Let [PreClosure] !G
  | LetRec [PreClosure] !G
  | Lit !Word64
  deriving Show

_Ref :: Prism' G Ref
_Ref = prism (\r -> App (Ref r) []) $ \case
  App (Ref r) [] -> Right r
  co             -> Left co

type PreClosure = ([Ref], LambdaForm)

type Tag = Word8

data Continuation = Cont (Map Tag (Conventional Word8, G)) (Maybe G)
  deriving Show

data Func = Ref Ref | Con Tag
  deriving Show

data LambdaForm = LambdaForm
  { _free   :: !(Conventional Word32)
  , _bound  :: !(Conventional Word8)
  , _update :: !Bool
  , _body   :: !G
  } deriving Show

makeLenses ''LambdaForm

noUpdate :: Conventional Word32 -> Conventional Word8 -> G -> LambdaForm
noUpdate f b e = LambdaForm f b False e

doUpdate :: Conventional Word32 -> G -> LambdaForm
doUpdate f e = LambdaForm f 0 True e

standardConstructor :: Conventional Word32 -> Tag -> LambdaForm
standardConstructor f t = LambdaForm f 0 False $ App (Con t) $ ifoldMap (\i n -> Local i <$> [0..n-1]) f
