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

import Ermine.Builtin.Pattern
import Ermine.Builtin.Core
import Ermine.Pretty
import Ermine.Pretty.Core
import Ermine.Syntax
import Ermine.Syntax.Core as Core
import Ermine.Syntax.Global
import Ermine.Syntax.Pattern
import Ermine.Syntax.Pattern.Compiler
import Ermine.Syntax.Term as Term hiding (Explicit)
import Ermine.Syntax.Type as Type

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

zipWithDef :: [([P (Annot k t) String], [(Maybe (Core String), Core String)])]
zipWithDef =
  [ (["f", conp consg ["x","xs"], conp consg ["y","ys"]],
      [(Nothing, Data 1 [apps "f" ["x","y"], apps "zipWith" ["f", "xs", "ys"]])])
  , ([_p, _p, _p],[(Nothing, Data 0 [])])
  ]

zipWithCompPretty = simpleEnv & do
                      c <- plamBranch zipWithDef
                      prettyCore names (-1) (const . pure . text) c

fooDef :: [([P (Annot k t) String], [(Maybe (Core String), Core String)])]
fooDef =
  [ ([conp nilg [], tup [_p, "x"]], [(Nothing, "x")])
  , ([conp consg ["x", _p], _p], [(Nothing, "x")])
  ]

fooCompPretty = simpleEnv & do
                  c <- plamBranch fooDef
                  prettyCore names (-1) (const . pure . text) c

filterDef :: [([P (Annot k t) String], [(Maybe (Core String), Core String)])]
filterDef =
  [ (["p", conp consg ["x", "xs"]],
        [ (Just (apps "p" ["x"]), Data 1 ["x", apps "filter" ["p", "xs"]])
        , (Nothing, apps "filter" ["p", "xs"])
        ])
  , ([_p, _p], [(Nothing, Data 0 [])])
  ]

filterCompPretty = simpleEnv & do
                     c <- plamBranch filterDef
                     prettyCore names (-1) (const . pure . text) c
