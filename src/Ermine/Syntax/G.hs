{-# LANGUAGE TemplateHaskell #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett and Dan Doel 2014
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
  , G(..)
  , _Ref
  , PreClosure(..)
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
import Ermine.Syntax.Sort

data Ref
  = Global !Word32
  | Local  !Word32
  | Stack  !Word32
  deriving Show

makePrisms ''Ref

data G
  = Case !G !Continuation
  | App !Func !(Sorted [Ref])
  | Let    [PreClosure] !G
  | LetRec [PreClosure] !G
  | Lit !Word64
  deriving Show

_Ref :: Prism' G Ref
_Ref = prism (\r -> App (Ref r) (Sorted [] [] [])) $ \ xs -> case xs of
  App (Ref r) (Sorted [] [] []) -> Right r
  co                            -> Left co

data PreClosure = PreClosure !(Sorted [Ref]) !LambdaForm
  deriving Show

type Tag = Word8

data Continuation = Cont (Map Tag (Sorted Word8, G)) (Maybe G)
  deriving Show

data Func
  = Ref !Ref -- closure ref
  | Con !Tag
  deriving Show

data LambdaForm = LambdaForm
  { _free   :: !(Sorted Word32)
  , _bound  :: !(Sorted Word32)
  , _update :: !Bool
  , _body   :: !G
  } deriving Show

makeLenses ''LambdaForm

noUpdate :: Sorted Word32 -> Sorted Word32 -> G -> LambdaForm
noUpdate f b e = LambdaForm f b False e

doUpdate :: Sorted Word32 -> G -> LambdaForm
doUpdate f e = LambdaForm f 0 True e

standardConstructor :: Sorted Word32 -> Tag -> LambdaForm
standardConstructor f t = LambdaForm f 0 False $ App (Con t) $ fmap (\n -> Local <$> [0..n-1]) f
