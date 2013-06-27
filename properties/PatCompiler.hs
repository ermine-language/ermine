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
import Ermine.Syntax.Term as Term hiding (Explicit)

nilg = glob Idfix "ermine" "Data.List" "Nil"
consg = glob Idfix "ermine" "Data.List" "Cons"
listSig = HM.fromList [(nilg, 0), (consg, 1)]

noneg = glob Idfix "ermine" "Data.Maybe" "Nothing"
someg = glob Idfix "ermine" "Data.Maybe" "Just"
maySig = HM.fromList [(noneg, 0), (someg, 1)]

leftg = glob Idfix "ermine" "Data.Either" "Left"
rightg = glob Idfix "ermine" "Data.Either" "Right"
eitherSig = HM.fromList [(leftg, 0), (rightg, 1)]

thisg = glob Idfix "ermine" "Data.Which" "This"
thatg = glob Idfix "ermine" "Data.Which" "That"
theseg = glob Idfix "ermine" "Data.Which" "These"
whichSig = HM.fromList [(thisg, 0), (thatg, 1), (theseg, 2)]

simpleEnv :: PCompEnv
simpleEnv = PCompEnv $ (listSig <$ listSig)
            `HM.union` (maySig <$ maySig)
            `HM.union` (eitherSig <$ eitherSig)
            `HM.union` (whichSig <$ whichSig)

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

zipWithCompPretty = simpleEnv & do
                      c <- compile zipWithInfo1 zipWithMatrix1
                      prettyCore nms (-1) zipWithK c
 where nms = filter (`notElem` ["f","l1","l2"]) names

fooCases = [ [ ConP nilg [], TupP [WildcardP,  SigP ()] ]
           , [ ConP consg [SigP (), WildcardP], WildcardP ]
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

fooCompPretty = simpleEnv & do c <- compile fooInfo fooMatrix ; prettyCore nms (-1) fooK c
 where nms = filter (`notElem` ["l","p"]) names

filterCases =
  [ [ SigP (),   ConP consg [SigP (), SigP ()] ]
  , [ WildcardP, ConP consg [SigP (), SigP ()] ]
  , [ WildcardP, ConP nilg [] ]
  ]

filterMatrix :: PMatrix () (Var Int String)
filterMatrix = PMatrix (transpose filterCases) [g, Trivial, Trivial]
  [ Scope $ Data 1 [x, apps fltr [p, xs]]
  , Scope $ apps fltr [p, xs]
  , Scope $ Data 0 []
  ]
 where
 g  = Explicit . Scope $ apps p [x]
 p  = pure . B $ ArgPP 0 LeafPP
 x  = pure . B . ArgPP 1 $ FieldPP 0 LeafPP
 xs = pure . B . ArgPP 1 $ FieldPP 1 LeafPP
 fltr = pure . F . pure . F $ "filter"

filterInfo :: CompileInfo (Var Int String)
filterInfo = CInfo (HM.fromList [ (ArgPP 0 LeafPP, pure . B $ 0)
                                , (ArgPP 1 LeafPP, pure . B $ 1)
                                ])
                   (fmap (pure . B) [0,1])
                   (fmap argPP [0,1])

filterK (B 0) _ = pure . text $ "p"
filterK (B 1) _ = pure . text $ "l"
filterK (F s) _ = pure . text $ s

filterCompPretty = simpleEnv & do
                     c <- compile filterInfo filterMatrix
                     prettyCore nms (-1) filterK c
 where nms = filter (`notElem` ["l","p"]) names
