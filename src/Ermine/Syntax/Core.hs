{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett and Dan Doel 2012-2014
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Ermine.Syntax.Core
  (
  -- * Core Terms
    Core(..)
  , Match(..)
  , matchArgs
  , matchGlobal
  , matchBody
  , Cored(..)
  , JavaLike(..)
  , Foreign(..)
  , HardCore(..)
  , AsHardCore(..)
  , super
  , slot
  -- * Smart constructors
  , lam
  , let_
  , dataCon
  , prim
  ) where

import Bound
import Bound.Var
import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Lens as Lens
import qualified Data.Binary as Binary
import Data.Binary (Binary)
import Data.Bytes.Get
import Data.Bytes.Put
import Data.Bytes.Serial
import Data.Data
import Data.List as List
import Data.Foldable
import Data.Hashable
import Data.Hashable.Extras
import qualified Data.Serialize as Serialize
import Data.Map
import Data.Serialize (Serialize)
import Data.String
import Data.Text as Strict hiding (cons, length)
import Data.Word
import Ermine.Syntax
import Ermine.Syntax.Convention
import Ermine.Syntax.Global as Global hiding (N)
import Ermine.Syntax.Id (Id, AsId(..))
import Ermine.Syntax.Literal
import Ermine.Syntax.Scope
import GHC.Generics
import Prelude.Extras
import Prelude

----------------------------------------------------------------------------
-- JavaLike
----------------------------------------------------------------------------

data JavaLike
  -- | Java methods: static, class name, method name, arg class names
  = Method !Bool !Strict.Text !Strict.Text [Strict.Text]
  -- | Java constructors: class name, arg class names
  | Constructor !Strict.Text [Strict.Text]
  -- | Java values: static, class name, field name
  | Value !Bool !Strict.Text !Strict.Text
  deriving (Eq,Ord,Show,Read,Data,Typeable,Generic)

instance Hashable JavaLike

instance Serial JavaLike where
  serialize (Method st cn mn args) = putWord8 0 >> serialize st >> serialize cn >> serialize mn >> serialize args
  serialize (Constructor cn args)  = putWord8 1 >> serialize cn >> serialize args
  serialize (Value st cn fn)       = putWord8 2 >> serialize st >> serialize cn >> serialize fn

  deserialize = getWord8 >>= \b -> case b of
    0 -> Method      <$> deserialize <*> deserialize <*> deserialize <*> deserialize
    1 -> Constructor <$> deserialize <*> deserialize
    2 -> Value       <$> deserialize <*> deserialize <*> deserialize
    _ -> fail $ "get JavaLike: Unexpected constructor code: " ++ show b

----------------------------------------------------------------------------
-- Foreign
----------------------------------------------------------------------------

data Foreign
  = JavaLike !JavaLike
  | Unknown !Strict.Text
  deriving (Eq,Ord,Show,Read,Data,Typeable,Generic)

instance Serial Foreign where
  serialize (JavaLike j) = putWord8 0 >> serialize j
  serialize (Unknown s)  = putWord8 1 >> serialize s

  deserialize = getWord8 >>= \b -> case b of
    0 -> liftM JavaLike deserialize
    1 -> liftM Unknown  deserialize
    _ -> fail $ "get Foreign: Unexpected constructor code: " ++ show b

instance Hashable Foreign

----------------------------------------------------------------------------
-- HardCore
----------------------------------------------------------------------------

-- | 'HardCore' is the subset of 'Core' terms that can be unified with value equality.
data HardCore
  = Super      !Word64
  | Slot       !Word64 -- slot# + the number of supers
  | Lit        !Literal
  | Foreign    !Foreign
  | Error      !Strict.Text
  | Id         !Id
  deriving (Eq,Ord,Show,Read,Data,Typeable,Generic)

-- | This class describes things that could be 'HardCore'.
class AsId c => AsHardCore c where
  _HardCore :: Prism' c HardCore

  _Lit :: Prism' c Literal
  _Lit = _HardCore._Lit

  _Error :: Prism' c Strict.Text
  _Error = _HardCore._Error

  _Super :: Prism' c Word64
  _Super = _HardCore._Super

  _Slot  :: Prism' c Word64
  _Slot = _HardCore._Slot

  _Foreign :: Prism' c Foreign
  _Foreign = _HardCore._Foreign

instance f ~ Core => AsGlobal (Scope b f a) where
  _Global = _Id._Global

instance f ~ Core => AsId (Scope b f a) where
  _Id = _HardCore._Id

instance f ~ Core => AsHardCore (Scope b f a) where
  _HardCore = prism (Scope . HardCore) $ \ t@(Scope b) -> case b of
    HardCore k           -> Right k
    Var (F (HardCore k)) -> Right k
    _                    -> Left t

instance AsGlobal HardCore where
  _Global     = _Id._Global

instance AsId HardCore where
  _Id         = prism Id         $ \ xs -> case xs of Id l      -> Right l ; hc -> Left hc

instance AsHardCore HardCore where
  _HardCore = id

  _Lit        = prism Lit        $ \ xs -> case xs of Lit     l -> Right l ; hc -> Left hc
  _Error      = prism Error      $ \ xs -> case xs of Error   l -> Right l ; hc -> Left hc
  _Super      = prism Super      $ \ xs -> case xs of Super   l -> Right l ; hc -> Left hc
  _Slot       = prism Slot       $ \ xs -> case xs of Slot    l -> Right l ; hc -> Left hc
  _Foreign    = prism Foreign    $ \ xs -> case xs of Foreign l -> Right l ; hc -> Left hc

instance Hashable HardCore

instance Serial HardCore where
  serialize (Super i)      = putWord8 0 >> serialize i
  serialize (Slot g)       = putWord8 1 >> serialize g
  serialize (Lit i)        = putWord8 2 >> serialize i
  serialize (Foreign f)    = putWord8 3 >> serialize f
  serialize (Error s)      = putWord8 4 >> serialize s
  serialize (Id g)         = putWord8 5 >> serialize g

  deserialize = getWord8 >>= \b -> case b of
    0 -> liftM Super   deserialize
    1 -> liftM Slot    deserialize
    2 -> liftM Lit     deserialize
    3 -> liftM Foreign deserialize
    4 -> liftM Error   deserialize
    5 -> liftM Id      deserialize
    _ -> fail $ "get HardCore: Unexpected constructor code: " ++ show b

instance Binary HardCore where
  put = serialize
  get = deserialize

instance Serialize HardCore where
  put = serialize
  get = deserialize

----------------------------------------------------------------------------
-- Cored
----------------------------------------------------------------------------

-- | Instances of this are things we can both construct from a 'Core' expression,
-- and perform case analysis upon.
--
-- e.g. 'Core' and @'Scope' b 'Core'@
class (Variable c, AppHash c, AppDict c, App c, Applicative c, Monad c) => Cored c where
  core :: Core a -> c a
  case_ :: c a -> Map Word64 (Match c a) -> Maybe (Scope () c a) -> c a
  caseLit :: Bool -> c a -> Map Literal (c a) -> Maybe (c a) -> c a
  lambda :: [Convention] -> Scope Word64 c a -> c a
  letrec :: [Scope Word64 c a] -> Scope Word64 c a -> c a
  hardCore :: HardCore -> c a
  hardCore = core . HardCore
  {-# INLINE hardCore #-}

instance Cored Core where
  core = id
  {-# INLINE core #-}
  case_ = Case
  caseLit = CaseLit
  lambda [] s = instantiate (error "lambda: impossible argument") s
  lambda cc s = Lam cc s
  letrec = Let

instance Cored m => Cored (Scope b m) where
  core = lift . core
  {-# INLINE core #-}
  case_ e bs d = Scope $ case_ (unscope e) (fmap (over matchBody expandScope) bs) (fmap expandScope d)
  caseLit n e bs d = Scope $ caseLit n (unscope e) (unscope <$> bs) (unscope <$> d)
  lambda cc e = Scope . lambda cc $ expandScope e
  letrec ds body = Scope $ letrec (expandScope <$> ds) (expandScope body)

expandScope :: forall t b1 b2 a. (Applicative t, Monad t)
            => Scope b2 (Scope b1 t) a -> Scope b2 t (Var b1 (t a))
-- Scope b1 t (Var b2 (Scope b1 t a))
-- t (Var b1 (Var b2 (Scope b1 t a)))
-- t (Var b2 (Var b1 (Scope b1 t a)))
-- t (Var b2 (t (Var b1 (t a))))
expandScope (Scope e) = Scope e'''
 where
 twist :: forall c d e. Var c (Var d e) -> Var d (Var c e)
 twist = unvar (F . B) (unvar B (F . F))
 e' :: t (Var b1 (Var b2 (Scope b1 t a)))
 e' = fromScope e
 e'' :: t (Var b2 (Var b1 (Scope b1 t a)))
 e'' = fmap twist e'
 e''' :: t (Var b2 (t (Var b1 (t a))))
 e''' = (fmap.fmap) (unvar (pure . B) unscope) e''

----------------------------------------------------------------------------
-- Match
----------------------------------------------------------------------------

data Match c a = Match
  { _matchArgs   :: [Convention]
  , _matchGlobal :: !Global
  , _matchBody   :: Scope Word64 c a
  } deriving (Eq,Show,Functor,Foldable,Traversable)

instance (Monad c, Hashable1 c, Hashable a) => Hashable (Match c a) where
  hashWithSalt n (Match cc g b) = (n `hashWithSalt` cc `hashWithSalt` g) `hashWithSalt1` b

instance Monad c => BoundBy (Match c) c where
  boundBy f (Match cc g b) = Match cc g (b >>>= f)

instance (Monad c, Hashable1 c) => Hashable1 (Match c)

instance Serial1 c => Serial1 (Match c) where
  serializeWith pa (Match cc g b) = serialize cc >> serialize g >> serializeWith pa b
  deserializeWith ga = liftM3 Match deserialize deserialize (deserializeWith ga)

instance (Serial1 c, Serial a) => Serial (Match c a) where
  serialize = serializeWith serialize
  deserialize = deserializeWith deserialize

instance (Serial1 c, Binary a) => Binary (Match c a) where
  put = serializeWith Binary.put
  get = deserializeWith Binary.get

instance (Serial1 c, Serialize a) => Serialize (Match c a) where
  put = serializeWith Serialize.put
  get = deserializeWith Serialize.get

matchArgs :: Lens' (Match c a) [Convention]
matchArgs f (Match a g b) = f a <&> \a' -> Match a' g b

matchGlobal :: Lens' (Match c a) Global
matchGlobal f (Match a g b) = f g <&> \g' -> Match a g' b

matchBody :: Lens (Match c a) (Match d b) (Scope Word64 c a) (Scope Word64 d b)
matchBody f (Match a g b) = Match a g <$> f b

----------------------------------------------------------------------------
-- Core
----------------------------------------------------------------------------

-- | 'Core' values are the output of the compilation process.
--
-- They are terms where the dictionary passing has been made explicit
-- and all of the types have been checked and removed.
data Core a
  = Var a
  | HardCore !HardCore
  | Data [Convention] !Word64 !Global [Core a] -- convention, tag #, associated global for display purposes, cores
  | Prim [Convention] Convention !Global [Core a]
  | App !Convention !(Core a) !(Core a)
  | Lam [Convention] !(Scope Word64 Core a)
  | Let [Scope Word64 Core a] !(Scope Word64 Core a)
  | Case !(Core a) (Map Word64 (Match Core a)) (Maybe (Scope () Core a))
  | Dict { supers :: [Core a], slots :: [Scope Word64 Core a] }
  | CaseLit !Bool !(Core a) (Map Literal (Core a)) (Maybe (Core a)) -- set True for native for strings
  deriving (Eq,Show,Functor,Foldable,Traversable)

instance AsGlobal (Core a) where
  _Global = _HardCore._Id._Global

instance AsId (Core a) where
  _Id = _HardCore._Id

instance AsHardCore (Core a) where
  _HardCore = prism HardCore $ \c -> case c of
    HardCore hc -> Right hc
    _           -> Left c

instance AppDict Core where
  _AppDict = prism (uncurry $ App D) $ \ xs -> case xs of App D f d -> Right (f, d) ; c -> Left c

instance AppHash Core where
  _AppHash = prism (uncurry $ App U) $ \ xs -> case xs of App U f d -> Right (f, d) ; c -> Left c


instance Serial1 Core where
  -- | Binary serialization of a 'Core', given serializers for its parameter.
  serializeWith pa (Var a)            = putWord8 0 >> pa a
  serializeWith _  (HardCore h)       = putWord8 1 >> serialize h
  serializeWith pa (Data cc i g cs)   = putWord8 2 >> serialize cc >> serialize i >> serialize g >> serializeWith (serializeWith pa) cs
  serializeWith pa (Prim cc r g cs)   = putWord8 3 >> serialize cc >> serialize r >> serialize g >> serializeWith (serializeWith pa) cs
  serializeWith pa (App cc c1 c2)     = putWord8 4 >> serialize cc >> serializeWith pa c1 >> serializeWith pa c2
  serializeWith pa (Lam cc s)         = putWord8 5 >> serialize cc >> serializeWith pa s
  serializeWith pa (Let ss s)         = putWord8 6 >> serializeWith (serializeWith pa) ss >> serializeWith pa s
  serializeWith pa (Case c bs d)      = putWord8 7 >> serializeWith pa c >> serializeWith (serializeWith pa) bs >> serializeWith (serializeWith pa) d
  serializeWith pa (Dict sups slts)   = putWord8 8 >> serializeWith (serializeWith pa) sups >> serializeWith (serializeWith pa) slts
  serializeWith pa (CaseLit n c bs d) = putWord8 9 >> serialize n >> serializeWith pa c >> serializeWith (serializeWith pa) bs >> serializeWith (serializeWith pa) d

  deserializeWith ga = getWord8 >>= \b -> case b of
    0 -> liftM Var ga
    1 -> liftM HardCore deserialize
    2 -> liftM4 Data deserialize deserialize deserialize (deserializeWith $ deserializeWith ga)
    3 -> liftM4 Prim deserialize deserialize deserialize (deserializeWith $ deserializeWith ga)
    4 -> liftM3 App deserialize (deserializeWith ga) (deserializeWith ga)
    5 -> liftM2 Lam deserialize (deserializeWith ga)
    6 -> liftM2 Let (deserializeWith (deserializeWith ga)) (deserializeWith ga)
    7 -> liftM3 Case (deserializeWith ga) (deserializeWith $ deserializeWith ga) (deserializeWith $ deserializeWith ga)
    8 -> liftM2 Dict (deserializeWith (deserializeWith ga)) (deserializeWith $ deserializeWith ga)
    9 -> liftM4 CaseLit deserialize (deserializeWith ga) (deserializeWith $ deserializeWith ga) (deserializeWith $ deserializeWith ga)
    _ -> fail $ "deserializeWith: Unexpected constructor code: " ++ show b

instance Serial a => Serial (Core a) where
  serialize = serializeWith serialize
  deserialize = deserializeWith deserialize

instance Binary a => Binary (Core a) where
  put = serializeWith Binary.put
  get = deserializeWith Binary.get

instance Serialize a => Serialize (Core a) where
  put = serializeWith Serialize.put
  get = deserializeWith Serialize.get

instance Hashable1 Core

-- | Distinct primes used for salting the hash.
distHardCore, distData, distPrim, distApp, distLam, distLet, distCase, distDict, distCaseLit :: Word
distHardCore = maxBound `quot` 3
distData     = maxBound `quot` 5
distPrim     = maxBound `quot` 7
distApp      = maxBound `quot` 11
distLam      = maxBound `quot` 13
distLet      = maxBound `quot` 17
distCase     = maxBound `quot` 19
distDict     = maxBound `quot` 23
distCaseLit  = maxBound `quot` 29

instance Hashable a => Hashable (Core a) where
  hashWithSalt n (Var a)             = hashWithSalt n a
  hashWithSalt n (HardCore c)        = hashWithSalt n c                                                      `hashWithSalt` distHardCore
  hashWithSalt n (Data cc i g cs)    = hashWithSalt n cc `hashWithSalt` i `hashWithSalt` g `hashWithSalt` cs `hashWithSalt` distData
  hashWithSalt n (Prim cc r g cs)    = hashWithSalt n cc `hashWithSalt` r `hashWithSalt` g `hashWithSalt` cs `hashWithSalt` distPrim
  hashWithSalt n (App cc x y)        = hashWithSalt n cc `hashWithSalt` x `hashWithSalt` y                   `hashWithSalt` distApp
  hashWithSalt n (Lam cc b)          = hashWithSalt n cc `hashWithSalt` b                                    `hashWithSalt` distLam
  hashWithSalt n (Let ts b)          = hashWithSalt n ts `hashWithSalt` b                                    `hashWithSalt` distLet
  hashWithSalt n (Case c bs d)       = hashWithSalt n c  `hashWithSalt` bs `hashWithSalt` d                  `hashWithSalt` distCase
  hashWithSalt n (Dict s ss)         = hashWithSalt n s  `hashWithSalt` ss                                   `hashWithSalt` distDict
  hashWithSalt n (CaseLit cc c bs d) = hashWithSalt n cc `hashWithSalt` c `hashWithSalt` bs `hashWithSalt` d `hashWithSalt` distCaseLit

instance IsString a => IsString (Core a) where
  fromString = Var . fromString

instance App Core where
  _App = prism (uncurry (App C)) $ \t -> case t of
    App C l r -> Right (l,r)
    _       -> Left t

instance Variable Core where
  _Var = prism Var $ \t -> case t of
    Var a -> Right a
    _     -> Left t
  {-# INLINE _Var #-}

instance Applicative Core where
  pure = Var
  (<*>) = ap

instance Monad Core where
  return = Var
  Var a            >>= f = f a
  HardCore h       >>= _ = HardCore h
  Data cc tg g xs  >>= f = Data cc tg g ((>>= f) <$> xs)
  Prim cc r g xs   >>= f = Prim cc r g ((>>= f) <$> xs)
  App cc x y       >>= f = App cc (x >>= f) (y >>= f)
  Lam cc e         >>= f = Lam cc (boundBy f e)
  Let bs e         >>= f = Let (boundBy f <$> bs) (boundBy f e)
  Case e as d      >>= f = Case (e >>= f) (over matchBody (boundBy f) <$> as) ((>>>= f) <$> d)
  Dict xs ys       >>= f = Dict ((>>= f) <$> xs) ((>>>= f) <$> ys)
  CaseLit c e as d >>= f = CaseLit c (e >>= f) ((>>= f) <$> as) ((>>= f) <$> d)

instance Eq1 Core
instance Show1 Core

----------------------------------------------------------------------------
-- Core Combinators
----------------------------------------------------------------------------

-- | ask for the @n@th the superclass of a given dictionary as a core expression
super :: (AsHardCore (c a), AppDict c) => Word64 -> c a -> c a
super i c = _AppDict # (_Super # i, c)

-- | ask for the @n@th slot of a given dictionary as a core expression
slot :: (AsHardCore (c a), AppDict c) => Word64 -> c a -> c a
slot i c = _AppDict # (_Slot # i, c)

-- | Smart 'Lam' constructor
lam :: (Cored m, Eq a) => Convention -> [a] -> Core a -> m a
lam c as t = core $ Lam (c <$ as) (abstract (fmap fromIntegral . flip List.elemIndex as) t)

-- | Smart 'Let' constructor
let_ :: (Cored m, Eq a) => [(a, Core a)] -> Core a -> m a
let_ bs b = core $ Let (abstr . snd <$> bs) (abstr b)
  where vs  = fst <$> bs
        abstr = abstract (fmap fromIntegral . flip List.elemIndex vs)

-- | Builds an n-ary data constructor
dataCon :: Cored m => [Convention] -> Word64 -> Global -> m a
dataCon [] tg g = core $ Data [] tg g []
dataCon cc tg g = core $ Lam cc $ Scope $ Data cc tg g $ pure.B <$> [0..fromIntegral (length cc-1)]

-- | Builds an n-ary primop
prim :: Cored m => [Convention] -> Convention -> Global -> m a
prim [] r g = core $ Prim [] r g []
prim cc r g = core $ Lam cc $ Scope $ Prim cc r g $ pure.B <$> [0..fromIntegral (length cc-1)]

----------------------------------------------------------------------------
-- Orphan Instances
----------------------------------------------------------------------------

instance (Hashable a, Hashable b) => Hashable (Map a b) where
  hashWithSalt n m = hashWithSalt n (Data.Map.toAscList m)

