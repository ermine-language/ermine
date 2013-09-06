{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Inference where

import Bound
import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.ST.Class
import Data.Maybe
import Data.Monoid
import Data.Map as Map
import Ermine.Builtin.Type
import Ermine.Inference.Discharge
import Ermine.Unification.Meta
import Ermine.Syntax
import Ermine.Syntax.Class
import Ermine.Syntax.Core hiding (App)
import Ermine.Syntax.Global
import Ermine.Syntax.Head
import Ermine.Syntax.Hint
import Ermine.Syntax.Id
import Ermine.Syntax.Instance
import Ermine.Syntax.Kind as Kind
import Ermine.Syntax.ModuleName
import Ermine.Syntax.Type as Type
import Ermine.Inference.Type
import Test.QuickCheck
import Test.QuickCheck.Property
import Test.Framework.TH
import Test.Framework.Providers.QuickCheck2

newtype DumbDischarge s a = DD { unDD :: M s (Maybe a) }

fooCon, barCon, bazCon :: Type k t
fooCon = con foo (schema $ constraint ~> star)
barCon = con bar (schema $ constraint ~> star)
bazCon = con baz (schema $ constraint ~> star)

foo, bar, baz :: Global
foo = glob Idfix (mkModuleName_ "Ermine") "Foo"
bar = glob Idfix (mkModuleName_ "Ermine") "Bar"
baz = glob Idfix (mkModuleName_ "Ermine") "Baz"

instance MonadST (DumbDischarge s) where
  type World (DumbDischarge s) = s
  liftST = DD . liftST . fmap Just

instance Monad (DumbDischarge s) where
  return = DD . return . return
  DD m >>= f = DD $ do
    ma <- m
    case ma of
      Just x -> unDD $ f x
      Nothing -> return Nothing

instance Functor (DumbDischarge s) where
  fmap = liftM

instance Applicative (DumbDischarge s) where
  pure = return
  (<*>) = ap

instance MonadMeta s (DumbDischarge s) where
  askMeta = DD $ Just <$> askMeta
  localMeta f (DD m) = DD $ localMeta f m

instance Alternative (DumbDischarge s) where
  empty = DD $ pure Nothing
  DD mma <|> dd = DD $ mma >>= \ma -> case ma of
    Nothing -> unDD dd
    _       -> pure ma

fooInstance = Instance [] (mkHead foo 0 [] [] [int]) (Dict [] [])
barInstance = Instance [] (mkHead bar 0 [] [] [int]) (Dict [fooInstance^.instanceBody] [])
bazInstance = Instance [] (mkHead baz 0 [] [] [int]) (Dict [barInstance^.instanceBody] [])

dumbDischargeEnv = DischargeEnv
                 { _classes = Map.fromList
                           [ (foo, Class [] [Unhinted $ Scope $ star] [])
                           , (baz, Class [] [Unhinted $ Scope $ star] [barCon `App` pure 0])
                           , (bar, Class [] [Unhinted $ Scope $ star] [fooCon `App` pure 0])
                           ]
                 , _instances = Map.fromList
                     [ (foo, [fooInstance])
                     , (bar, [barInstance])
                     , (baz, [bazInstance])
                     ]
                 }

instance MonadDischarge s (DumbDischarge s) where
  askDischarge = return dumbDischargeEnv
  localDischarge _ m = m

runDD :: (forall s. DumbDischarge s a) -> Maybe a
runDD dd = either (\_ -> Nothing) id $ runM mempty (unDD dd)

prop_discharge_optimal = let v = pure () in
  forAll (oneof [ pure [App barCon v, App bazCon v]
                , pure [App bazCon v, App barCon v]
                ]) $ \sups ->
    fromMaybe False $ runDD $ do
      c :: Scope () Core (Type () ()) <- App fooCon v `dischargesBySupers` sups
      return $ c == super 0 (super 0 . pure $ App bazCon v)

prop_entails_optimal = let v = pure () in
  forAll (oneof [ pure [App barCon v, App bazCon v]
                , pure [App bazCon v, App barCon v]
                ]) $ \sups ->
    fromMaybe False $ runDD $ do
      c :: Scope () Core (Type () ()) <- sups `entails` App fooCon v
      return $ c == super 0 (super 0 . pure $ App bazCon v)

prop_instance_discharge = maybe False (const True) $ runDD $ do
  dischargesByInstance (App fooCon int :: Type () ())

tests = $testGroupGenerator
