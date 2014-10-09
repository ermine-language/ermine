{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett and Dan Doel 2014
-- License   :  BSD2
-- Maintainer:  Dan Doel <dan.doel@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Ermine.Syntax.G
  ( Native
  , Ref(..)
  , _Global
  , _Local
  , _Stack
  , _Lit
  , _Native
  , _UnsafeNative
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
  , dictionary
  ) where

import Control.Lens
import Data.Foldable as F
import Data.Word
import Data.Map hiding (update)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Ermine.Syntax.Sort
import Ermine.Syntax.Id
import GHC.Prim (Any)
import Unsafe.Coerce

type Native = Any

data Ref
  = Global !Id
  | Local  !Word64
  | Stack  !Word64
  | Lit    !Word64
  | Native !Native

instance Show Ref where
  showsPrec n (Global i) = showParen (n>10) $ showString "Global " . showsPrec 11 i
  showsPrec n (Local  w) = showParen (n>10) $ showString "Local " . shows w
  showsPrec n (Stack  w) = showParen (n>10) $ showString "Stack " . shows w
  showsPrec n (Lit    w) = showParen (n>10) $ showString "Lit " . shows w
  showsPrec _ (Native _) = showString "{Native}"

makePrisms ''Ref

_UnsafeNative :: Prism' Ref a
_UnsafeNative = prism (Native . unsafeCoerce) $ \r -> case r of
  Native a -> Right $ unsafeCoerce a
  s        -> Left s
data G
  = Case !G !Continuation
  | CaseLit !Ref !Continuation
  | App !(Sorted Word64) !Func !(Sorted (Vector Ref))
  | Let    (Vector PreClosure) !G
  | LetRec (Vector PreClosure) !G
  | Slot
  deriving Show

_Ref :: Sorted Word64 -> Prism' G Ref
_Ref w = prism (\r -> App w (Ref r) $ return V.empty) $ \ xs -> case xs of
  App w' (Ref r) s | w == w' && F.all V.null s -> Right r
  co                                           -> Left co

data PreClosure = PreClosure !(Sorted (Vector Ref)) !LambdaForm
  deriving Show

type Tag = Word64

data Continuation = Continuation (Map Tag (Sorted Word64, G)) (Maybe G) deriving Show

data Func
  = Ref  !Ref -- closure ref
  | Con  !Tag
  deriving Show

data LambdaForm = LambdaForm
  { _free   :: !(Sorted Word64)
  , _bound  :: !(Sorted Word64)
  , _update :: !Bool
  , _body   :: !G
  } deriving Show

makeLenses ''LambdaForm

noUpdate :: Sorted Word64 -> Sorted Word64 -> G -> LambdaForm
noUpdate f b e = LambdaForm f b False e

doUpdate :: Sorted Word64 -> G -> LambdaForm
doUpdate f e = LambdaForm f 0 True e

standardConstructor :: Sorted Word64 -> Tag -> LambdaForm
standardConstructor f t = LambdaForm f 0 False $ App 0 (Con t) $ fmap (\i -> V.generate (fromIntegral i) $ Local . fromIntegral) f

dictionary :: Word64 -> LambdaForm
dictionary f = LambdaForm (Sorted f 0 0) (Sorted 0 1 0) False Slot
