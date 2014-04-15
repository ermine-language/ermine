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
--  , _Ref
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
import Data.Word
import Data.Map hiding (update)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Ermine.Syntax.Sort

data Ref
  = Global !Word32
  | Local  !Word32
  | Stack  !Word32
  deriving Show

makePrisms ''Ref

data G
  = Case !G !Continuation
  | CaseLit Ref !Continuation
  | App !(Sorted Word32) !Func !(Sorted (Vector Ref))
  | Let (Vector PreClosure) !G
  | LetRec (Vector PreClosure) !G
  | Lit !Word64
  deriving Show

{-
_Ref :: Sorted Word32 -> Prism' G Ref
_Ref w = prism (\r -> App w (Ref r) $ return V.empty) $ \ xs -> case xs of
  App w' (Ref r) s | w == w' && F.all V.null s -> Right r
  co                                          -> Left co
-}

data PreClosure = PreClosure !(Sorted (Vector Ref)) !LambdaForm
  deriving Show

type Tag = Word64

data Continuation = Continuation (Map Tag (Sorted Word8, G)) (Maybe G) deriving Show

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
standardConstructor f t = LambdaForm f 0 False $ App 0 (Con t) $ fmap (\i -> V.generate (fromIntegral i) $ Local . fromIntegral) f
