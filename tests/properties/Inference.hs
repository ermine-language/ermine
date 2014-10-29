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
import Ermine.Constraint.Env
import Ermine.Constraint.Simplification
import Ermine.Unification.Meta
import Ermine.Syntax
import Ermine.Syntax.Convention (Convention)
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

newtype DD s a = DD { unDD :: M s (Maybe a) }

fooCon, barCon, bazCon :: Type k t
fooCon = con foo (constraint ~> star)
barCon = con bar (constraint ~> star)
bazCon = con baz (constraint ~> star)

foo, bar, baz :: Global
foo = glob Idfix (mkModuleName_ "Ermine") "Foo"
bar = glob Idfix (mkModuleName_ "Ermine") "Bar"
baz = glob Idfix (mkModuleName_ "Ermine") "Baz"

instance MonadST (DD s) where
  type World (DD s) = s
  liftST = DD . liftST . fmap Just

instance Monad (DD s) where
  return = DD . return . return
  DD m >>= f = DD $ do
    ma <- m
    case ma of
      Just x -> unDD $ f x
      Nothing -> return Nothing

instance Functor (DD s) where
  fmap = liftM

instance Applicative (DD s) where
  pure = return
  (<*>) = ap

instance MonadMeta s (DD s) where
  askMeta = DD $ Just <$> askMeta
  localMeta f (DD m) = DD $ localMeta f m

instance Alternative (DD s) where
  empty = DD $ pure Nothing
  DD mma <|> dd = DD $ mma >>= \ma -> case ma of
    Nothing -> unDD dd
    _       -> pure ma

fooInstance = Instance [] (mkHead foo 0 [] [] [int]) (Dict [] [])
barInstance = Instance [] (mkHead bar 0 [] [] [int]) (Dict [fooInstance^.instanceBody] [])
bazInstance = Instance [] (mkHead baz 0 [] [] [int]) (Dict [barInstance^.instanceBody] [])

dumbConstraintEnv = ConstraintEnv
                 { _classes = Map.fromList
                           [ (foo, Class [] [(Nothing, Scope star)] [] Map.empty Map.empty)
                           , (baz, Class [] [(Nothing, Scope star)] [view (from trivialTK) $ barCon `App` pure 0] Map.empty Map.empty)
                           , (bar, Class [] [(Nothing, Scope star)] [view (from trivialTK) $ fooCon `App` pure 0] Map.empty Map.empty)
                           ]
                 , _instances = Map.fromList
                     [ (foo, [fooInstance])
                     , (bar, [barInstance])
                     , (baz, [bazInstance])
                     ]
                 }

instance MonadConstraint Convention s (DD s) where
  type Cv (DD s) = Convention
  askConstraint = return dumbConstraintEnv
  localConstraint _ m = m

runDD :: (forall s. DD s a) -> Maybe a
runDD dd = either (\_ -> Nothing) id $ runM mempty (unDD dd)

prop_discharge_optimal = let v = pure () in
  forAll (oneof [ pure [App barCon v, App bazCon v]
                , pure [App bazCon v, App barCon v]
                ]) $ \sups ->
    fromMaybe False $ runDD $ do
      c :: Scope () (Core Convention) (Type () ()) <- App fooCon v `bySupers` sups
      return $ c == super 0 (super 0 . pure $ App bazCon v)

prop_entails_optimal = let v = pure () in
  forAll (oneof [ pure [App barCon v, App bazCon v]
                , pure [App bazCon v, App barCon v]
                ]) $ \sups ->
    fromMaybe False $ runDD $ do
      c :: Scope () (Core Convention) (Type () ()) <- sups `entails` App fooCon v
      return $ c == super 0 (super 0 . pure $ App bazCon v)

prop_instance_discharge = maybe False (const True) $ runDD $ do
  byInstance (App fooCon int :: Type () ())

tests = $testGroupGenerator
