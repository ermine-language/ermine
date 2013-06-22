{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module PatCompiler where

import Bound
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
  isSignature gs = DPC $ gs == fromList [nilg, consg]
  constructorTag g
    | g == nilg  = pure 0
    | g == consg = pure 1
    | otherwise  = error "tag: Unknown constructor"
  constructorArity g
    | g == nilg  = pure 0
    | g == consg = pure 2
    | otherwise  = error "arity: Unknown constructor"


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
