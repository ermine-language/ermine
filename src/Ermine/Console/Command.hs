{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett and Dan Doel 2012-2013
-- License   :  BSD2
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
import Bound.Var (unvar)
import Control.Applicative
import Control.Lens
import Control.Monad (liftM, (<=<))
import Control.Monad.Error.Class
import Control.Monad.IO.Class
import Control.Monad.State (StateT(..), evalStateT)
import Control.Monad.State.Class
import Data.Bifunctor
import Data.Bitraversable
import Data.Char
import Data.Default
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Int (Int32, Int64)
import Data.List as List
import Data.List.NonEmpty (NonEmpty(..))
import Data.List.Split (splitOn)
import Data.Set (notMember)
import Data.Set.Lens
import Data.Semigroup
import Data.Traversable (for, forM)
import Data.Text (Text, unpack, pack)
import qualified Data.Text.IO as Text
import Data.Foldable (for_, forM_)
import Data.Word (Word64)
import Data.Void
import Ermine.Builtin.Core (cPutStrLn, cShowInt, cShowLong, cShowLongHash, cAddLong, cFromIntegerToInt, cFromIntegerToLong)
import Ermine.Builtin.Global (putStrLng, showIntg, showLongg, addLongg, literalg, fromIntegerToIntg, fromIntegerToLongg)
import Ermine.Builtin.Head
import Ermine.Builtin.Term as Term (dataCon)
import Ermine.Builtin.Type as Type (lame, maybe_, ee, io, string, long, longh, int, integer, fromInteg)
import Ermine.Console.State
import Ermine.Constraint.Env
import Ermine.Core.Optimizer
import Ermine.Inference.Kind as Kind
import Ermine.Inference.Type as Type
import Ermine.Interpreter as Interp
import Ermine.Core.Compiler
import Ermine.Loader.Core (Loader, fanoutIdLoaderM, load,
                           thenM, thenM')
import Ermine.Loader.Filesystem (filesystemLoader, Freshness, LoadRefusal)
import Ermine.Loader.MapCache (loadCached)
import Ermine.Parser.Data
import Ermine.Parser.Kind
import Ermine.Parser.Module
import Ermine.Parser.State
import Ermine.Parser.Type
import Ermine.Parser.Term
import Ermine.Pretty hiding (string, int, integer)
import Ermine.Pretty.G
import Ermine.Pretty.Core
import Ermine.Pretty.Kind
import Ermine.Pretty.Type
import Ermine.Pretty.Term
import Ermine.Syntax
import Ermine.Syntax.Convention
import Ermine.Syntax.Data (DataType)
import Ermine.Syntax.Core as Core
import Ermine.Syntax.Global as Global
import Ermine.Syntax.Id
import Ermine.Syntax.Kind as Kind
import Ermine.Syntax.Literal (Literal(Long, Int))
import Ermine.Syntax.Module (Import, Module, ModuleHead, importModule,
                             moduleName, moduleHeadImports, moduleHeadText)
import Ermine.Syntax.ModuleName (ModuleName)
import Ermine.Syntax.Name
import Ermine.Syntax.Scope
import Ermine.Syntax.Type as Type
import Ermine.Syntax.Term as Term
import Ermine.Unification.Kind
import Ermine.Unification.Meta
import Ermine.Version
import Prelude hiding (mapM)
import System.Console.Haskeline
import System.Exit
import Text.Groom
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
  , _body    :: [String] -> String -> Console ()
  }

makeClassy ''Command

cmd :: String -> Command
cmd nm = Command nm [] Nothing Nothing "" $ \_ _ -> return ()

getCommand :: String -> Maybe (Command, [String], String)
getCommand zs = commands ^?
    folded.
    filtered (\c -> isPrefixOf xs (c^.cmdName)
                 || anyOf (alts.folded) (isPrefixOf xs) c).
    to (,as,ys')
  where
    (cs, ys) = break isSpace zs
    xs:as = splitOn "+" cs
    ys' = reverse $ dropWhile isSpace $ reverse $ dropWhile isSpace ys

executeCommand :: String -> Console ()
executeCommand txt = case getCommand txt of
  Just (c,args,input)  -> view body c args input
  Nothing          -> do
    sayLn $ text "ermine: error: Unknown command:" <+> text (show txt)
    showHelp [] txt

showHelp :: [String] -> String -> Console ()
showHelp _ _ = sayLn $ vsep (map format commands) where
  format c = fill 18 (withArg c) <+> hang 18 (fillSep (text <$> words (c^.desc)))
  withArg c = case c^.arg of
    Nothing -> bold (char ':' <> text (c^.cmdName))
    Just a  -> bold (char ':' <> text (c^.cmdName)) <+> angles (text a)

------------------------------------------------------------------------------
-- commands
------------------------------------------------------------------------------

procArgs :: [String] -> NonEmpty (String, a) -> a
procArgs args (x :| xs) = foldl f (snd x) args
 where
 m = x:xs
 f r s | Just a <- lookup s m = a
       | otherwise            = r

parsing :: Parser a -> ([String] -> a -> Console ())
        -> [String] -> String -> Console ()
parsing p k args s = case parseString (p <* eof) mempty s of
  Success a   -> k args a
  Failure doc -> sayLn doc

-- TODO generalize with parsing
parsingS :: StateT s Parser a -> ([String] -> a -> Console ())
        -> [String] -> String -> StateT s Console ()
parsingS p k args s = StateT $ \st ->
  let p' = runStateT p st
  in case parseString (p' <* eof) mempty s of
      Success (a, st') -> k args a <&> (,st')
      Failure doc      -> sayLn doc <&> (,st)

moduleLoader :: forall e m. (MonadError Doc m,
                             MonadState (HashMap ModuleName (e, Module)) m)
             => Loader e m ModuleName Text
             -> Loader e m ModuleName Module
moduleLoader l = fanoutIdLoaderM lemh `thenM'` afterModuleHead
  where lemh :: Loader e m ModuleName (ModuleHead Import Text)
        lemh = l `thenM` parseModuleHead
        dep :: ModuleName -> m (ModuleHead Import (e, Text))
        dep = liftM strength . (lemh^.load)
        importSet :: [ModuleHead Import (e, Text)] -> m ()
        importSet mhs = do
          preceding <- get
          -- Here we finally use the invariant of 'pickImportPlan':
          -- the 'Module's referenced by 'Import's *must* already be
          -- in the state.
          let precImp i = (i, snd $ preceding HM.! (i^.importModule))
          emods <- mhs `forM` \mh ->
              (mh^.moduleHeadText._1,)
              `liftM` parseModuleRemainder (bimap precImp snd mh)
          put (foldl' (\hm em@(_, md) -> HM.insert (md^.moduleName) em hm)
                      preceding emods)
        afterModuleHead (e, (n, mh)) = do
          already <- get
          impss <- pickImportPlan dep already n
                 . strength $ (e, mh)
          impss `forM_` importSet
          gets (HM.! n)

-- | Make a plan to import dependencies, based on importing a single
-- dependency.  Include all the module heads and remaining bodies in
-- the plan.
--
-- Invariant: For each element of the result, every 'Import'
-- referenced by each 'ModuleHead' appears in a previous element of
-- the result.
pickImportPlan :: MonadError Doc m
               => (ModuleName -> m (ModuleHead Import txt))
                  -- ^ load a module head by name
               -> HashMap ModuleName a -- ^ already-loaded Modules
               -> ModuleName           -- ^ initial module name
               -> ModuleHead Import txt -- ^ initial module head
               -> m [[ModuleHead Import txt]]
pickImportPlan dep hm n fstMH = do
  graph <- fetchGraphM deps (const . dep) (HM.singleton n fstMH)
  topSort graph deps &
    either (throwError . reportCircle) (return . fmap (fmap snd))
  where deps :: ModuleHead Import a -> [ModuleName]
        deps = filter (\mn -> not (HM.member mn hm))
             . toListOf (moduleHeadImports.folded.importModule)
        reportCircle circ = "Circular dependency detected: "
                         <> pretty (circ <&> pretty . unpack . (^.name))

-- Why isn't this defined?
strength :: Functor f => (a, f b) -> f (a, b)
strength (a, fb) = (a,) <$> fb

parseModuleHead :: MonadError Doc m => Text -> m (ModuleHead Import Text)
parseModuleHead = asMError . parseString (moduleHead <* eof) mempty . unpack

parseModuleRemainder :: MonadError Doc m
                     => ModuleHead (Import, Module) Text
                     -> m Module
parseModuleRemainder mh =
  mh ^. moduleHeadText
    & unpack
    & parseString (wholeModule mh <* eof) mempty
    & asMError

asMError :: MonadError Doc m => Result a -> m a
asMError (Success a) = return a
asMError (Failure doc) = throwError doc

kindBody :: [String] -> Type (Maybe Text) (Var Text Text) -> Console ()
kindBody args ty = do
  gk <- ioM mempty $ do
    tm <- kindVars (maybe (newMeta False Nothing) pure)
      <=< bimemoverse (traverse $ newMeta False . Just)
                      (fmap pure . newMeta False . unvar Just Just)
        $ ty
    k <- inferKind id tm
    generalize k
  disp gk
 where
 disp = procArgs args $
         ("pretty", sayLn . flip prettySchema names . vacuous)
      :| [("ugly", sayLn . text . groom)]

dkindsBody :: [String] -> [DataType () Text] -> Console ()
dkindsBody _ dts = do
  ckdts <- ioM mempty (checkDataTypeKinds dts)
  for_ ckdts $ \ckdt ->
    sayLn $ text (unpack $ ckdt^.name)
        <+> colon
        <+> prettySchema (vacuous $ schema ckdt) names

checkAndCompile :: MonadConstraint (KindM s) s m
                => Term Ann Text -> m (Maybe (Type t k, Core Convention c))
checkAndCompile syn = traverse resolveGlobals (syn >>= predefs) `for` \syn' -> do
  tm <- bitraverse (memoverse
                          (\s -> newMeta False Nothing >>=
                                   flip newMeta (Just s) . pure))
                 pure
                 syn'
  w <- inferType 0 fst $ first (first absurd) tm
  over _2 (>>= snd) <$> generalizeType w
 where
 clame :: Type k t
 clame = con lame (star ~> constraint)
 tyLame :: Type k t
 tyLame = Forall [] [(Nothing,Scope star)]
            (Scope $ apps clame [pure $ B 0]) (Scope . pure $ B 0)
 cfromInteger :: Type k t
 cfromInteger = con fromInteg (star ~> constraint)
 tyFromInteger :: Type k t
 tyFromInteger = Forall [] [(Nothing,Scope star)]
                   (Scope $ apps cfromInteger [pure $ B 0])
                   (Scope $ integer ~> (pure $ B 0))
 tyPSL :: Type k t
 tyPSL = string ~> io (tuple 0)
 tySI6 :: Type k t
 tySI6 = long ~> string
 tySI6H :: Type k t
 tySI6H = longh ~> string
 tySI :: Type k t
 tySI = int ~> string
 tyAL :: Type k t
 tyAL = long ~> long ~> long
 predefs "Nothing" =
   Term.dataCon Idfix "Ermine" "Nothing" $
     Forall [] [(Nothing, Scope star)]
            (Scope $ And []) (Scope . maybe_ . pure $ B 0)
 predefs "Just"    =
   Term.dataCon Idfix "Ermine" "Just" $
     Forall [] [(Nothing, Scope star)]
            (Scope $ And []) (Scope $ pure (B 0) ~> maybe_ (pure $ B 0))
 predefs "E"       =
   Term.dataCon Idfix "Ermine" "E" $
     Forall [] [(Nothing, Scope star)]
            (Scope $ And []) (Scope $ pure (B 0) ~> ee)
 predefs  x = pure x
 resolveGlobals :: AsConvention cc => Text -> Maybe (Type t k, Core cc c)
 resolveGlobals txt
   | txt == "lame" = Just (tyLame, HardCore $ Slot 0)
   | txt == "fromInteger" = Just (tyFromInteger, HardCore $ Slot 0)
   | txt == "putStrLn" = Just (tyPSL, cPutStrLn)
   | txt == "showInt" = Just (tySI, cShowInt)
   | txt == "showLong" = Just (tySI6, cShowLong)
   | txt == "showLongHash" = Just (tySI6H, cShowLongHash)
   | txt == "addLong" = Just (tyAL, cAddLong)
 resolveGlobals _ = Nothing

typeBody :: [String] -> Term Ann Text -> Console ()
typeBody args syn = ioM mempty (runCM (checkAndCompile syn) dummyConstraintEnv) >>= \ xs -> case xs of
  Just (ty, _) -> disp ty
  Nothing -> sayLn "Unbound variables detected"
 where
 disp = procArgs args $
         ("pretty", \ty -> sayLn $ prettyType ty names (-1))
      :| [("ugly", sayLn . text . groom)]

coreBody :: [String] -> Term Ann Text -> Console ()
coreBody args syn = ioM mempty (runCM (checkAndCompile syn) dummyConstraintEnv) >>= \ xs -> case xs of
  Just (_, c) -> disp (opt c)
  Nothing     -> sayLn "Unbound variables detected"
 where
 disp = procArgs args $
         ("pretty", sayLn . runIdentity . prettyCore names (-1) (const.pure))
      :| [("ugly", sayLn . text . groom)]
 opt = procArgs args $ ("opt", optimize) :| [("noopt", id)]

gBody :: [String] -> Term Ann Text -> Console ()
gBody args syn = ioM mempty (runCM (checkAndCompile syn) dummyConstraintEnv) >>= \xs ->
  case xs of
    Just (_, c) -> disp $ compile 0 absurd (opt c)
    Nothing     -> sayLn "Unbound variables detected"
 where
 disp = procArgs args $
         ("pretty", sayLn . prettyG (text <$> names) defaultCxt)
      :| [("ugly", sayLn . text . groom)]
 opt = procArgs args $ ("opt", optimize) :| [("noopt", id)]

echoBody :: [String] -> String -> Console ()
echoBody args =
  case procArgs args $ ("term", "term") :| [("type", "type"), ("kind", "kind")] of
    "kind" -> parsing kind (\_ s -> disp $ Kind.general s Just) args
     where
     disp = procArgs args $
              ("pretty", sayLn . (prettySchema ?? names))
          :| [("ugly", liftIO . putStrLn . groom)]
    "type" -> parsing typ (const $ disp . Type.abstractAll Just (unvar Just Just)) args
     where
     disp = procArgs args $
              ("pretty", pt)
          :| [("ugly", liftIO . putStrLn . groom . fst)]
     pt (tsch, hs) = let stsch = hoistScope (first ("?" <$)) tsch
                      in sayLn $ prettyTypeSchema stsch hs names
    _ {- "term" -} -> flip evalStateT initialParserState -- TODO thread new state through
                      . parsingS term (\_ tm -> disp tm) args
     where
     disp = procArgs args $
              ("pretty", pt)
          :| [("ugly", liftIO . putStrLn . groom)]
     pt tm = prettyTerm
               tm names' (-1) (error "TODO: prettyAnn") (pure.pure.text.unpack)
         >>= sayLn
      where names' = filter ((`notMember` setOf traverse tm).pack) names

evalBody :: [String] -> Term Ann Text -> Console ()
evalBody args syn =
  ioM mempty (runCM (checkAndCompile syn) dummyConstraintEnv) >>= \case
    Just (_, c) -> liftIO $ do
      psl <- allocPrimOp $ primOpNZ Text.putStrLn
      si6 <- allocPrimOp . primOpUN $
               return . pack . show . (fromIntegral :: Word64 -> Int64)
      si <- allocPrimOp . primOpUN $
               return . pack . show . (fromIntegral :: Word64 -> Int32)
      al <- allocPrimOp . primOpUUU $ (+)
      dli <- allocGlobal (error "evalBody: dli") $
               Dict [] [Scope $ Data [U] 0 literalg [_Lit # Int 5]]
      dll <- allocGlobal (error "evalBody: dll") $
               Dict [] [Scope $ Data [U] 0 literalg [_Lit # Long 37]]
      dfii <- allocGlobal (error "evalBody: dfii") $
               Dict [] [Scope $ cFromIntegerToInt ]
      dfil <- allocGlobal (error "evalBody: dfil") $
               Dict [] [Scope $ cFromIntegerToLong ]
      fi2i <- allocPrimOp . primOpNU $
        return . fromIntegral . (fromInteger :: Integer -> Int32)
      fi2l <- allocPrimOp . primOpNU $
        return . fromIntegral . (fromInteger :: Integer -> Int64)
      ms <- defaultMachineState 512 $ HM.fromList
              [ (_Global # putStrLng, psl)
              , (_Global # showIntg, si)
              , (_Global # showLongg, si6)
              , (_Global # addLongg, al)
              , (_Global # fromIntegerToIntg, fi2i)
              , (_Global # fromIntegerToLongg, fi2l)
              , (InstanceId hlameL, dll)
              , (InstanceId hlameI, dli)
              , (InstanceId hfromIntegerI, dfii)
              , (InstanceId hfromIntegerL, dfil)
              ]
      eval (compile 0 absurd . opt $ c) def (ms & trace .~ debug)
    Nothing -> return ()
 where
 opt = procArgs args $ ("opt", optimize) :| [("noopt", id)]
 debug = procArgs args $ ("nodebug", const $ return ()) :| [("debug", putStrLn)]

commands :: [Command]
commands =
  [ cmd "help" & desc .~ "show help" & alts .~ ["?"] & body .~ showHelp
  , cmd "quit" & desc .~ "quit" & body.mapped .~ const (liftIO exitSuccess)
  , cmd "kind" & desc .~ "infer the kind of a type"
      & body .~ parsing typ kindBody
  , cmd "type" & desc .~ "infer the type of a term"
      & body .~ (\args s -> evalStateT (parsingS term typeBody args s)
                                       initialParserState)
  , cmd "core" & desc .~ "dump the core representation of a term after type checking."
      & body .~ (\args s -> evalStateT (parsingS term coreBody args s)
                                       initialParserState)
  , cmd "g"
      & desc .~ "dump the sub-core representation of a term after type checking."
      & body .~ (\args s -> evalStateT (parsingS term gBody args s)
                                       initialParserState)
  , cmd "dkinds"
      & desc .~ "determine the kinds of a series of data types"
      & body .~ parsing (semiSep1 dataType) dkindsBody
  , cmd "echo"
      & desc .~ "parse and print an expression at any level"
      & body .~ echoBody
  , cmd "eval"
      & desc .~ "type check and evaluate a term"
      & body .~ (\args s -> evalStateT (parsingS term evalBody args s)
                                       initialParserState)
  -- , cmd "udata"
  --     & desc .~ "show the internal representation of a data declaration"
  --     & body .~ parsing dataType (liftIO . putStrLn . groom)
  -- , cmd "load" & arg  ?~ "filename" & desc .~ "load a file" & body .~ \xs -> liftIO $ putStrLn =<< readFile xs
  , cmd "version"
      & desc .~ "show the compiler version number"
      & body .~ \_ _ -> liftIO $ putStrLn version

  ]
