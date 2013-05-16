{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Inference where

import Bound
import Control.Applicative
import Control.Monad
import Control.Monad.ST.Class
import Data.Maybe
import Data.Monoid
import Ermine.Unification.Meta
import Ermine.Syntax
import Ermine.Syntax.Core hiding (App)
import Ermine.Syntax.Global
import Ermine.Syntax.Kind as Kind
import Ermine.Syntax.Type as Type
import Ermine.Inference.Type
import Test.QuickCheck.Property
import Test.Framework.TH
import Test.Framework.Providers.QuickCheck2

newtype DumbDischarge s a = DD { unDD :: M s (Maybe a) }

fooCon, barCon, bazCon :: Type k t
fooCon = con foo (schema $ constraint ~> star)
barCon = con bar (schema $ constraint ~> star)
bazCon = con baz (schema $ constraint ~> star)

foo, bar, baz :: Global
foo = glob Idfix "ermine" "Ermine" "Foo"
bar = glob Idfix "ermine" "Ermine" "Bar"
baz = glob Idfix "ermine" "Ermine" "Baz"

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

instance MonadDischarge s (DumbDischarge s) where
  superclasses (App (HardType (Con b _)) v)
    | b == baz  = pure $ [barCon `App` v]
    | b == bar  = pure $ [fooCon `App` v]
    | otherwise = empty

runDD :: (forall s. DumbDischarge s a) -> Maybe a
runDD dd = either (\_ -> Nothing) id $ runM mempty (unDD dd)

prop_discharge_optimal = expectFailure . fromMaybe (label "no result" failed) $ runDD $ do
  m <- newMeta star
  let x = pure m
  c <- App fooCon x `dischargesBySupers` [App barCon x, App bazCon x]
  d <- App fooCon x `dischargesBySupers` [App bazCon x, App barCon x]
  pure . conjoin $
    [ label "bar baz" $ c == super 0 (super 0 . pure . F $ App bazCon x)
    , label "baz bar" $ d == super 0 (super 0 . pure . F $ App bazCon x)
    ]

tests = $testGroupGenerator
