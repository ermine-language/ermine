{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett and Dan Doel 2012-2013
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Ermine.Console.Command
  ( Command(..)
  , HasCommand(..)
  , commands
  , executeCommand
  ) where

import Bound
import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Data.Bifunctor
import Data.Bitraversable
import Data.Char
import Data.List as List
import Data.Set (notMember)
import Data.Set.Lens
import Data.Semigroup
import Data.Traversable (for)
import Data.Text (Text, unpack, pack)
import Data.Foldable (for_)
import Data.Void
import Ermine.Builtin.Term as Term (dataCon)
import Ermine.Builtin.Type as Type (lame, maybe_, ee)
import Ermine.Console.State
import Ermine.Constraint.Env
import Ermine.Inference.Kind as Kind
import Ermine.Inference.Type as Type
import Ermine.Optimizer
import Ermine.Parser.Data
import Ermine.Parser.Kind
import Ermine.Parser.Type
import Ermine.Parser.Term
import Ermine.Pretty
import Ermine.Pretty.Core
import Ermine.Pretty.Kind
import Ermine.Pretty.Type
import Ermine.Pretty.Term
import Ermine.Syntax
import Ermine.Syntax.Data (DataType, dataTypeSchema)
import Ermine.Syntax.Core as Core
import Ermine.Syntax.Global as Global
import Ermine.Syntax.Hint
import Ermine.Syntax.Kind as Kind
import Ermine.Syntax.Name
import Ermine.Syntax.Scope
import Ermine.Syntax.Type as Type
import Ermine.Syntax.Term as Term
import Ermine.Unification.Kind
import Ermine.Unification.Meta
import Ermine.Version
import System.Console.Haskeline
import System.Exit
import Text.Parser.Combinators (eof)
import Text.Parser.Token (semiSep1)
import Text.Trifecta.Parser
import Text.Trifecta.Result

------------------------------------------------------------------------------
-- Command
------------------------------------------------------------------------------

data Command = Command
  { _cmdName :: String
  , _alts    :: [String]
  , _arg     :: Maybe String
  , _tabbed  :: Maybe (CompletionFunc Console)
  , _desc    :: String
  , _body    :: String -> Console ()
  }

makeClassy ''Command

cmd :: String -> Command
cmd nm = Command nm [] Nothing Nothing "" $ \_ -> return ()

getCommand :: String -> Maybe (Command, String)
getCommand zs = commands ^?
    folded.
    filtered (\c -> isPrefixOf xs (c^.cmdName)
                 || anyOf (alts.folded) (isPrefixOf xs) c).
    to (,ys')
  where
    (xs, ys) = break isSpace zs
    ys' = reverse $ dropWhile isSpace $ reverse $ dropWhile isSpace ys

executeCommand :: String -> Console ()
executeCommand txt = case getCommand txt of
  Just (c,args)  -> view body c args
  Nothing          -> do
    sayLn $ text "ermine: error: Unknown command:" <+> text (show txt)
    showHelp txt

showHelp :: String -> Console ()
showHelp _ = sayLn $ vsep (map format commands) where
  format c = fill 18 (withArg c) <+> hang 18 (fillSep (text <$> words (c^.desc)))
  withArg c = case c^.arg of
    Nothing -> bold (char ':' <> text (c^.cmdName))
    Just a  -> bold (char ':' <> text (c^.cmdName)) <+> angles (text a)

------------------------------------------------------------------------------
-- commands
------------------------------------------------------------------------------

parsing :: Parser a -> (a -> Console ()) -> String -> Console ()
parsing p k s = case parseString (p <* eof) mempty s of
  Success a   -> k a
  Failure doc -> sayLn doc

kindBody :: Type (Maybe Text) Text -> Console ()
kindBody s = do
  gk <- ioM mempty $ do
    tm <- prepare (newMeta ())
                  (const $ newMeta ())
                  (const $ pure <$> newMeta ())
                  s
    k <- inferKind tm
    generalize k
  sayLn $ prettySchema (vacuous gk) names

dkindsBody :: [DataType () Text] -> Console ()
dkindsBody dts = do
  ckdts <- ioM mempty (checkDataTypeKinds dts)
  for_ ckdts $ \ckdt ->
    sayLn $ text (unpack $ ckdt^.name)
        <+> colon
        <+> prettySchema (vacuous $ dataTypeSchema ckdt) names

checkAndCompile :: MonadConstraint s m
                => Term Ann Text -> m (Maybe (Type t k, Core c))
checkAndCompile syn = traverse closedOrLame (syn >>= predefs) `for` \syn' -> do
  tm <- bitraverse (prepare (newMeta ())
                          (const $ newMeta ())
                          (const $ newMeta () >>= newMeta . pure))
                 pure
                 syn'
  w <- inferType 0 (const tyLame) tm
  over _2 join <$> generalizeType w
 where
 clame :: Type k t
 clame = con lame (star ~> constraint)
 tyLame :: Type k t
 tyLame = Forall [] [Unhinted $ Scope star]
            (Scope $ apps clame [pure $ B 0]) (Scope . pure $ B 0)
 predefs "Nothing" =
   Term.dataCon Idfix "Ermine" "Nothing" $
     Forall [] [Unhinted $ Scope star]
            (Scope $ And []) (Scope . maybe_ . pure $ B 0)
 predefs "Just"    =
   Term.dataCon Idfix "Ermine" "Just" $
     Forall [] [Unhinted $ Scope star]
            (Scope $ And []) (Scope $ pure (B 0) ~> maybe_ (pure $ B 0))
 predefs "E"       =
   Term.dataCon Idfix "Ermine" "E" $
     Forall [] [Unhinted $ Scope star]
            (Scope $ And []) (Scope $ pure (B 0) ~> ee)
 predefs  x = pure x
 closedOrLame :: Text -> Maybe (Core c)
 closedOrLame txt | txt == "lame" =
   Just (HardCore $ Slot 0)
 closedOrLame _ = Nothing

typeBody :: Term Ann Text -> Console ()
typeBody syn = ioM mempty (runCM (checkAndCompile syn) dummyConstraintEnv) >>= \ xs -> case xs of
  Just (ty, _) -> sayLn $ prettyType ty names (-1)
  Nothing -> sayLn "Unbound variables detected"

coreBody :: Term Ann Text -> Console ()
coreBody syn = ioM mempty (runCM (checkAndCompile syn) dummyConstraintEnv) >>= \ xs -> case xs of
  Just (_, c) -> sayLn . runIdentity $ prettyCore names (-1) const (optimize c)
  Nothing           -> sayLn "Unbound variables detected"

commands :: [Command]
commands =
  [ cmd "help" & desc .~ "show help" & alts .~ ["?"] & body .~ showHelp
  , cmd "quit" & desc .~ "quit" & body.mapped .~ liftIO exitSuccess
  , cmd "ukind"
      & desc .~ "show the internal representation of a kind schema"
      & body .~ parsing kind (liftIO . print . (Kind.general ?? stringHint))
  , cmd "utype"
      & desc .~ "show the internal representation of a type"
      & body .~ parsing typ (liftIO . print . fst . Type.abstractAll stringHint stringHint)
  , cmd "pkind"
      & desc .~ "show the pretty printed representation of a kind schema"
      & body .~ parsing kind (\s -> sayLn $ prettySchema (Kind.general s stringHint) names)
  , cmd "ptype"
      & desc .~ "show the pretty printed representation of a type schema"
      & body .~ parsing typ (\s ->
                  let (tsch, hs) = abstractAll stringHint stringHint s
                      stsch = hoistScope (first ("?" <$)) tsch
                   in sayLn $ prettyTypeSchema stsch hs names)
  , cmd "kind" & desc .~ "infer the kind of a type"
      & body .~ parsing typ kindBody
  , cmd "type" & desc .~ "infer the type of a term"
      & body .~ parsing term typeBody
  , cmd "core" & desc .~ "dump the core representation of a term after type checking."
      & body .~ parsing term coreBody
  , cmd "dkinds"
      & desc .~ "determine the kinds of a series of data types"
      & body .~ parsing (semiSep1 dataType) dkindsBody
  , cmd "uterm"
      & desc .~ "show the internal representation of a term"
      & body .~ parsing term (liftIO . print)
  , cmd "pterm"
      & desc .~ "show the pretty printed representation of a term"
      & body .~ parsing term (\tm ->
                  let names' = filter ((`notMember` setOf traverse tm).pack) names in
                  prettyTerm tm names' (-1) (error "TODO: prettyAnn")
                             (pure . pure . text . unpack)
                    >>= sayLn)
  , cmd "udata"
      & desc .~ "show the internal representation of a data declaration"
      & body .~ parsing dataType (liftIO . print)
  -- , cmd "load" & arg  ?~ "filename" & desc .~ "load a file" & body .~ \xs -> liftIO $ putStrLn =<< readFile xs
  , cmd "version"
      & desc .~ "show the compiler version number"
      & body .~ \_ -> liftIO $ putStrLn version

  ]
