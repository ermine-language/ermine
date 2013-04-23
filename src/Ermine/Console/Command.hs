{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExtendedDefaultRules #-}
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

import Control.Applicative
import Control.Lens
import Control.Monad.IO.Class
import Data.Bifunctor
import qualified Data.ByteString.Char8 as SB
import Data.Char
import Data.List
import Data.Set (notMember)
import Data.Set.Lens
import Data.Semigroup
import Data.Void
import Ermine.Console.State
import Ermine.Inference.Kind
import Ermine.Parser.DataType
import Ermine.Parser.Kind
import Ermine.Parser.Type
import Ermine.Parser.Term
import Ermine.Pretty
import Ermine.Pretty.Kind
import Ermine.Pretty.Type
import Ermine.Pretty.Term
import qualified Ermine.Syntax.DataType as DataType
import Ermine.Syntax.DataType (DataType)
import Ermine.Syntax.Global as Global
import Ermine.Syntax.Hint
import Ermine.Syntax.Kind as Kind
import Ermine.Syntax.Scope
import Ermine.Syntax.Type as Type
import Ermine.Unification.Kind
import Ermine.Unification.Meta
import System.Console.Haskeline
import System.Exit
import Text.Trifecta.Parser
import Text.Trifecta.Result

------------------------------------------------------------------------------
-- Command
------------------------------------------------------------------------------

data Command = Command
  { _name   :: String
  , _alts   :: [String]
  , _arg    :: Maybe String
  , _tabbed :: Maybe (CompletionFunc Console)
  , _desc   :: String
  , _body   :: String -> Console ()
  }

makeClassy ''Command

cmd :: String -> Command
cmd nm = Command nm [] Nothing Nothing "" $ \_ -> return ()

getCommand :: String -> Maybe (Command, String)
getCommand zs = commands ^?
    folded.
    filtered (\c -> isPrefixOf xs (c^.name) || anyOf (alts.folded) (isPrefixOf xs) c).
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
    Nothing -> bold (char ':' <> text (c^.name))
    Just a  -> bold (char ':' <> text (c^.name)) <+> angles (text a)

------------------------------------------------------------------------------
-- commands
------------------------------------------------------------------------------

parsing :: Parser a -> (a -> Console ()) -> String -> Console ()
parsing p k s = case parseString p mempty s of
  Success a   -> k a
  Failure doc -> sayLn doc

kindBody :: Type (Maybe String) String -> Console ()
kindBody s = do
  gk <- ioM mempty $ do
    tm <- prepare (newMeta ())
                  (const $ newMeta ())
                  (const $ pure <$> newMeta ())
                  s
    k <- inferKind tm
    generalize k
  sayLn $ prettySchema (vacuous gk) names

dkindBody :: DataType (Maybe String) String -> Console ()
dkindBody dt = do
  let nm = SB.unpack $ dt ^. DataType.name . globalName
  s <- ioM mempty $ do
    self <- newMeta ()
    dt' <- prepare (newMeta ())
                   (const $ newMeta ())
                   (\t -> if t == nm then pure $ pure self else pure <$> newMeta ())
                   dt
    checkDataTypeKind (pure self) dt'
  sayLn $ prettySchema s names

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
  , cmd "dkind"
      & desc .~ "check that a data type is well-kinded"
      & body .~ parsing dataType dkindBody
  , cmd "uterm"
      & desc .~ "show the internal representation of a term"
      & body .~ parsing term (liftIO . print)
  , cmd "pterm"
      & desc .~ "show the pretty printed representation of a term"
      & body .~ parsing term (\tm ->
                  let names' = filter (`notMember` setOf traverse tm) names in
                  prettyTerm tm names' (-1) (error "TODO: prettyAnn") (pure . pure . text)
                    >>= sayLn)
  , cmd "udata"
      & desc .~ "show the internal representation of a data declaration"
      & body .~ parsing dataType (liftIO . print)
  -- , cmd "load" & arg  ?~ "filename" & desc .~ "load a file" & body .~ \xs -> liftIO $ putStrLn =<< readFile xs

  ]
