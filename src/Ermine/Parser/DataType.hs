
--------------------------------------------------------------------
-- |
-- Copyright :  (c) Dan Doel 2013
-- License   :  BSD3
-- Maintainer:  Dan Doel <dan.doel@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
-- This module provides the parser for types
--------------------------------------------------------------------
module Ermine.Parser.DataType
  ( constr
  , dataType
  ) where

import Bound
import Control.Applicative
import Data.Bifunctor
import Data.List
import Data.Maybe
import Ermine.Parser.Global
import Ermine.Parser.Style
import Ermine.Parser.Type
import Ermine.Syntax.DataType
import Ermine.Syntax.Hint
import Ermine.Syntax.Type
import Text.Parser.Combinators
import Text.Parser.Token

abstractSimple :: Eq v => [v] -> v -> Var Int v
abstractSimple l v = maybe (F v) B $ elemIndex v l

constr :: (Monad m, TokenParsing m) => m (Constructor (Maybe String) String)
constr = build . fromMaybe ([], [])
     <$> optional (quantifier "forall")
     <*> globalIdent termCon
     <*> many typ
 where
 build (ks, ts) g as = Constructor g (map stringHint ks) ts' as'
  where
  ts' = map (\(tn, tk) -> abstract (`elemIndex` map Just ks) tk <$ stringHint tn) ts
  as' = abstract (`elemIndex` map fst ts) . abstractKinds (`elemIndex` map Just ks) <$> as

dataType :: (Monad m, TokenParsing m) => m (DataType (Maybe String) String)
dataType = build <$ symbol "data"
       <*> globalIdent typeCon
       <*> (fmap (fromMaybe []) . optional . braces . some $ ident kindIdent)
       <*> typVarBindings
       <*> sepBy constr (symbolic '|')
 where
 build nm ks ts cs = DataType nm (map stringHint ks) ts' cs'
  where
  jks = map Just ks
  ts' = map (\(tn, tk) -> abstract (`elemIndex` jks) tk <$ stringHint tn) ts
  cs' = bimap (abstractSimple jks) (abstractSimple $ map fst ts) <$> cs

