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
  , G(..)
  , _Ref
  , PreClosure
  , Tag
  , Continuation(..)
  , Func(..)
  , LambdaForm(LambdaForm)
  , freeArity
  , boundArity
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

data Ref = Global Word32 | Local Word32
  deriving Show

makePrisms ''Ref

data G
  = Case G Continuation
  | App Func [Ref]
  | Let [PreClosure] G
  | LetRec [PreClosure] G
  | Lit Word64
  deriving Show

_Ref :: Prism' G Ref
_Ref = prism (\r -> App (Ref r) []) $ \case
  App (Ref r) [] -> Right r
  co             -> Left co

type PreClosure = ([Ref], LambdaForm)

type Tag = Word8

data Continuation = Cont (Map Tag (Word8, G)) (Maybe G)
  deriving Show

data Func = Ref Ref | Con Tag
  deriving Show

data LambdaForm = LambdaForm
  { _freeArity :: Word32
  , _boundArity :: Word8
  , _update :: Bool
  , _body :: G
  } deriving Show

makeLenses ''LambdaForm

noUpdate :: Word32 -> Word8 -> G -> LambdaForm
noUpdate f b e = LambdaForm f b False e

doUpdate :: Word32 -> G -> LambdaForm
doUpdate f e = LambdaForm f 0 True e

standardConstructor :: Word32 -> Tag -> LambdaForm
standardConstructor w t = LambdaForm w 0 False . App (Con t) $ Local <$> [0..w-1]

