{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Ermine.Console.Command
  ( Command(..)
  , HasCommand(..)
  , commands
  , executeCommand
  ) where

import Control.Applicative
import Control.Lens
import Control.Monad.IO.Class
import Data.Char
import Data.List
import Data.Void
import Data.Semigroup
import Ermine.Console.State
import Ermine.Inference.Kind
import Ermine.Parser.Kind
import Ermine.Parser.Type
import Ermine.Parser.Term
import Ermine.Pretty
import Ermine.Pretty.Kind
import Ermine.Pretty.Type
import Ermine.Syntax.Kind as Kind
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
  pk <- prettySchema gk names absurd
  sayLn pk

commands :: [Command]
commands =
  [ cmd "help" & desc .~ "show help" & alts .~ ["?"] & body .~ showHelp
  , cmd "quit" & desc .~ "quit" & body.mapped .~ liftIO exitSuccess
  , cmd "ukind"
      & desc .~ "show the internal representation of a kind schema"
      & body .~ parsing kind (liftIO . print . Kind.general)
  , cmd "utype"
      & desc .~ "show the internal representation of a type"
      & body .~ parsing typ (liftIO . print . fst . Type.abstractAll)
  , cmd "pkind"
      & desc .~ "show the pretty printed representation of a kind schema"
      & body .~ parsing kind (\s -> prettySchema (Kind.general s) names absurd >>= sayLn)
  , cmd "ptype"
      & desc .~ "show the pretty printed representation of a type schema"
      & body .~ parsing typ (\s ->
                  uncurry prettyTypeSchema (abstractAll s) names
                          (\_ _ -> pure (text "?")) absurd
                    >>= sayLn)
  , cmd "kind" & desc .~ "infer the kind of a type"
      & body .~ parsing typ kindBody
  , cmd "uterm"
      & desc .~ "show the internal representation of a term"
      & body .~ parsing term (liftIO . print)
  -- , cmd "load" & arg  ?~ "filename" & desc .~ "load a file" & body .~ \xs -> liftIO $ putStrLn =<< readFile xs

  ]
