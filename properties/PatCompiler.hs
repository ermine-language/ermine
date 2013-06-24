{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module PatCompiler where

import Prelude hiding (any)
import Bound
import Control.Lens
import Control.Applicative
import Data.Foldable hiding (notElem)
import qualified Data.HashMap.Lazy as HM
import Data.List (transpose)
import Data.Set as Set hiding (notElem, filter)
import Data.Traversable

import Ermine.Pretty
import Ermine.Pretty.Core
import Ermine.Syntax
import Ermine.Syntax.Core as Core
import Ermine.Syntax.Global
import Ermine.Syntax.Pattern
import Ermine.Syntax.Pattern.Compiler
import Ermine.Syntax.Term as Term

newtype DumbPComp a = DPC { runDPC :: a }
  deriving (Show, Eq, Ord, Functor, Traversable, Foldable)

instance Applicative DumbPComp where
  pure = DPC
  DPC f <*> DPC x = DPC $ f x

instance Monad DumbPComp where
  return = DPC
  DPC x >>= g = g x

nilg :: Global
nilg = glob Idfix "ermine" "Data.List" "Nil"

consg :: Global
consg = glob Idfix "ermine" "Data.List" "Cons"

instance MonadPComp DumbPComp where
  isSignature gs = DPC $ gs == fromList [ConH 0 nilg, ConH 2 consg] || tupleSig
   where tupleSig = any (has _TupH) gs
  constructorTag (ConH _ g)
    | g == nilg  = pure 0
    | g == consg = pure 1
    | otherwise  = error "tag: Unknown constructor"
  constructorTag (TupH _) = pure 0


zipWithCases1 :: [[Pattern ()]]
zipWithCases1 = [ [ SigP (),   ConP consg [SigP (), SigP ()], ConP consg [SigP (), SigP ()] ]
                , [ WildcardP, WildcardP,                      WildcardP ]
                ]

zipWithMatrix1 :: PMatrix () (Var Int String)
zipWithMatrix1 = PMatrix (transpose zipWithCases1) [Trivial, Trivial]
  [ Scope $
    Data 1 [ apps (pure . B $ ArgPP 0 LeafPP)
              [pure . B $ ArgPP 1 (FieldPP 0 LeafPP), pure . B $ ArgPP 2 (FieldPP 0 LeafPP)]
           , apps (pure . F . pure . F $ "zipWith")
               [ pure . B $ ArgPP 0 LeafPP
               , pure . B $ ArgPP 1 (FieldPP 1 LeafPP)
               , pure . B $ ArgPP 2 (FieldPP 1 LeafPP)
               ]
           ]
  , Scope $ Data 0 []
  ]

zipWithInfo1 :: CompileInfo (Var Int String)
zipWithInfo1 = CInfo (HM.fromList [(ArgPP 0 LeafPP, pure . B $ 0)
                                  ,(ArgPP 1 LeafPP, pure . B $ 1)
                                  ,(ArgPP 2 LeafPP, pure . B $ 2)
                                  ])
                     (fmap (pure . B) [0..2])
                     (fmap argPP [0..2])

zipWithK :: Applicative f => Var Int String -> Int -> f Doc
zipWithK (B 0) _ = pure . text $ "f"
zipWithK (B 1) _ = pure . text $ "l1"
zipWithK (B 2) _ = pure . text $ "l2"
zipWithK (B _) _ = error "zipWithK: bad bound reference"
zipWithK (F s) _ = pure . text $ s

zipWithCompPretty = runDPC $ prettyCore nms (-1) zipWithK =<<
                      compile zipWithInfo1 zipWithMatrix1
 where nms = filter (`notElem` ["f","l1","l2"]) names

fooCases = [ [ ConP nilg [], TupP [WildcardP,  SigP ()] ]
           , [ ConP consg [WildcardP, SigP ()], WildcardP ]
           ]

fooMatrix = PMatrix (transpose fooCases) [Trivial, Trivial]
  [ Scope . pure . B $ ArgPP 1 (FieldPP 1 LeafPP)
  , Scope . pure . B $ ArgPP 0 (FieldPP 1 LeafPP)
  ]

fooInfo = CInfo (HM.fromList [ (ArgPP 0 LeafPP, pure . B $ 0)
                             , (ArgPP 1 LeafPP, pure . B $ 1)
                             ])
                (fmap (pure . B) [0,1])
                (fmap argPP [0,1])

fooK (B 0) _ = pure . text $ "l"
fooK (B 1) _ = pure . text $ "p"
fooK (F s) _ = pure . text $ s

fooCompPretty = runDPC $ prettyCore nms (-1) fooK =<< compile fooInfo fooMatrix
 where nms = filter (`notElem` ["l","p"]) names
