{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Pattern where

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
import Ermine.Syntax.Convention
import Ermine.Syntax.Core as Core
import Ermine.Syntax.Global
import Ermine.Syntax.ModuleName
import Ermine.Syntax.Pattern
import Ermine.Pattern.Env
import Ermine.Pattern.Matching
import Ermine.Pattern.Matrix
import Ermine.Syntax.Term as Term hiding (Explicit)
import Ermine.Syntax.Type as Type

nilg = glob Idfix (mkModuleName_ "Data.List") "Nil"
consg = glob Idfix (mkModuleName_ "Data.List") "Cons"
listSig = HM.fromList [(nilg, ([],0)), (consg, ([C,C],1))]

noneg = glob Idfix (mkModuleName_ "Data.Maybe") "Nothing"
someg = glob Idfix (mkModuleName_ "Data.Maybe") "Just"
maySig = HM.fromList [(noneg, ([C],0)), (someg, ([C],1))]

leftg = glob Idfix (mkModuleName_ "Data.Either") "Left"
rightg = glob Idfix (mkModuleName_ "Data.Either") "Right"
eitherSig = HM.fromList [(leftg, ([C],0)), (rightg, ([C],1))]

thisg = glob Idfix (mkModuleName_ "Data.Which") "This"
thatg = glob Idfix (mkModuleName_ "Data.Which") "That"
theseg = glob Idfix (mkModuleName_ "Data.Which") "These"
whichSig = HM.fromList [(thisg, ([C], 0)), (thatg, ([C], 1)), (theseg, ([C,C],2))]

simpleEnv :: PatternEnv
simpleEnv = PatternEnv $ (listSig <$ listSig)
            `HM.union` (maySig <$ maySig)
            `HM.union` (eitherSig <$ eitherSig)
            `HM.union` (whichSig <$ whichSig)

type CCS = Core Convention String

zipWithDef :: [([P (Annot k t) String], [(Maybe (CCS), CCS)])]
zipWithDef =
  [ (["f", conp consg ["x","xs"], conp consg ["y","ys"]],
      [(Nothing, Data [C] 1 consg [apps "f" ["x","y"], apps "zipWith" ["f", "xs", "ys"]])])
  , ([_p, _p, _p],[(Nothing, Data [] 0 nilg [])])
  ]

{-
zipWithCompPretty = simpleEnv & do
                      c <- plamBranch zipWithDef
                      prettyCore names (-1) (const . pure . text) c
-}

fooDef :: [([P (Annot k t) String], [(Maybe (CCS), CCS)])]
fooDef =
  [ ([conp nilg [], tup [_p, "x"]], [(Nothing, "x")])
  , ([conp consg ["x", _p], _p], [(Nothing, "x")])
  ]

{-
fooCompPretty = simpleEnv & do
                  c <- plamBranch fooDef
                  prettyCore names (-1) (const . pure . text) c
-}

filterDef :: [([P (Annot k t) String], [(Maybe (CCS), CCS)])]
filterDef =
  [ (["p", conp consg ["x", "xs"]],
        [ (Just (apps "p" ["x"]), Data [C] 1 consg ["x", apps "filter" ["p", "xs"]])
        , (Nothing, apps "filter" ["p", "xs"])
        ])
  , ([_p, _p], [(Nothing, Data [] 0 nilg [])])
  ]

{-
filterCompPretty = simpleEnv & do
                     c <- plamBranch filterDef
                     prettyCore names (-1) (const . pure . text) c
-}
