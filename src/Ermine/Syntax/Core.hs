{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett and Dan Doel 2012-2014
-- License   :  BSD2
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
  , triverseMatch
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
import Bound.Scope
import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Lens as Lens
import qualified Data.Binary as Binary
import Data.Binary (Binary)
import Data.Bifoldable
import Data.Bitraversable
import Data.Bytes.Get
import Data.Bytes.Put
import Data.Bytes.Serial
import Data.Data
import Data.List as List
import Data.Functor.Classes
import Data.Foldable
import Data.Hashable
import Data.Hashable.Lifted
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

instance f ~ Core t => AsGlobal (Scope b f a) where
  _Global = _Id._Global

instance f ~ Core t => AsId (Scope b f a) where
  _Id = _HardCore._Id

instance f ~ Core t => AsHardCore (Scope b f a) where
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
class (Variable c, AppHash c, AppDict c, App c, Applicative c, Monad c) => Cored t c | c -> t where
  core :: Core t a -> c a
  case_ :: c a -> Map Word64 (Match t c a) -> Maybe (Scope () c a) -> c a
  lambda :: [t] -> Scope Word64 c a -> c a
  letrec :: [Scope Word64 c a] -> Scope Word64 c a -> c a
  hardCore :: HardCore -> c a
  hardCore = core . HardCore
  {-# INLINE hardCore #-}

instance AsConvention t => Cored t (Core t) where
  core = id
  {-# INLINE core #-}
  case_ = Case
  lambda [] s = instantiate (error "lambda: impossible argument") s
  lambda cc s = Lam cc s
  letrec = Let

instance Cored t m => Cored t (Scope b m) where
  core = lift . core
  {-# INLINE core #-}
  case_ e bs d = Scope $ case_ (unscope e) (fmap (over matchBody expandScope) bs) (fmap expandScope d)
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

data Match t c a = Match
  { _matchArgs   :: [t]
  , _matchGlobal :: !Global
  , _matchBody   :: Scope Word64 c a
  } deriving (Eq,Show,Functor,Foldable,Traversable,Generic,Generic1)

triverseMatch
  :: Applicative f
  => (t -> f t') -> (forall x y. (x -> f y) -> c x -> f (c' y)) -> (a -> f a')
  -> Match t c a -> f (Match t' c' a')
triverseMatch f g h (Match cc gl e) =
  (Match ?? gl) <$> traverse f cc <*> bitransverseScope g h e

instance (Monad c, Hashable t, Hashable1 c, Hashable a) => Hashable (Match t c a) where
  hashWithSalt n (Match cc g b) = (n `hashWithSalt` cc `hashWithSalt` g) `hashWithSalt1` b

instance Monad c => BoundBy (Match t c) c where
  boundBy f (Match cc g b) = Match cc g (b >>>= f)

instance (Hashable t, Monad c, Hashable1 c) => Hashable1 (Match t c)

serializeMatch :: MonadPut m => (t -> m ()) -> (forall b. (b -> m ()) -> c b -> m ()) -> (a -> m ()) -> Match t c a -> m ()
serializeMatch pt pc pa (Match cc g b) = serializeWith pt cc >> serialize g >> serializeScope3 serialize pc pa b

deserializeMatch :: MonadGet m => m t -> (forall b. m b -> m (c b)) -> m a -> m (Match t c a)
deserializeMatch gt gc ga = Match <$> deserializeWith gt <*> deserialize <*> deserializeScope3 deserialize gc ga

instance (Serial t, Serial1 c) => Serial1 (Match t c) where
  serializeWith pa (Match cc g b) = serialize cc >> serialize g >> serializeWith pa b
  deserializeWith ga = liftM3 Match deserialize deserialize (deserializeWith ga)

instance (Serial t, Serial1 c, Serial a) => Serial (Match t c a) where
  serialize = serializeWith serialize
  deserialize = deserializeWith deserialize

instance (Serial t, Serial1 c, Binary a) => Binary (Match t c a) where
  put = serializeWith Binary.put
  get = deserializeWith Binary.get

instance (Serial t, Serial1 c, Serialize a) => Serialize (Match t c a) where
  put = serializeWith Serialize.put
  get = deserializeWith Serialize.get

matchArgs :: Lens (Match t c a) (Match t' c a) [t] [t']
matchArgs f (Match a g b) = f a <&> \a' -> Match a' g b

matchGlobal :: Lens' (Match t c a) Global
matchGlobal f (Match a g b) = f g <&> \g' -> Match a g' b

matchBody :: Lens (Match t c a) (Match t d b) (Scope Word64 c a) (Scope Word64 d b)
matchBody f (Match a g b) = Match a g <$> f b

----------------------------------------------------------------------------
-- Core
----------------------------------------------------------------------------

-- | 'Core' values are the output of the compilation process.
--
-- They are terms where the dictionary passing has been made explicit
-- and all of the types have been checked and removed.
data Core t a
  = Var a
  | HardCore !HardCore
  | Data [t] !Word64 !Global [Core t a] -- convention, tag #, associated global for display purposes, cores
  | Prim [t] t !Global [Core t a]
  | App !t !(Core t a) !(Core t a)
  | Lam [t] !(Scope Word64 (Core t) a)
  | Let [Scope Word64 (Core t) a] !(Scope Word64 (Core t) a)
  | Case !(Core t a) (Map Word64 (Match t (Core t) a)) (Maybe (Scope () (Core t) a))
  | Dict { supers :: [Core t a], slots :: [Scope Word64 (Core t) a] }
  deriving (Eq,Show,Functor,Foldable,Traversable,Generic,Generic1)

instance AsGlobal (Core t a) where
  _Global = _HardCore._Id._Global

instance AsId (Core t a) where
  _Id = _HardCore._Id

instance AsHardCore (Core t a) where
  _HardCore = prism HardCore $ \c -> case c of
    HardCore hc -> Right hc
    _           -> Left c

_AppConvention :: AsConvention k => Convention -> Prism' (Core k a) (Core k a, Core k a)
_AppConvention cc = prism (uncurry $ App (_Convention # cc)) $ \ xs -> case xs of
   App cc' f d | has (_Convention.only cc) cc' -> Right (f, d)
   _ -> Left xs

instance AsConvention t => AppDict (Core t) where
  _AppDict = _AppConvention D

instance AsConvention t => AppHash (Core t) where
  _AppHash = _AppConvention U

instance Serial2 Core where
  -- | Binary serialization of a 'Core', given serializers for its parameter.
  serializeWith2 _  pa (Var a)            = putWord8 0 >> pa a
  serializeWith2 _  _  (HardCore h)       = putWord8 1 >> serialize h
  serializeWith2 pt pa (Data cc i g cs)   = putWord8 2 >> serializeWith pt cc >> serialize i >> serialize g >> serializeWith (serializeWith2 pt pa) cs
  serializeWith2 pt pa (Prim cc r g cs)   = putWord8 3 >> serializeWith pt cc >> pt r >> serialize g >> serializeWith (serializeWith2 pt pa) cs
  serializeWith2 pt pa (App cc c1 c2)     = putWord8 4 >> pt cc >> serializeWith2 pt pa c1 >> serializeWith2 pt pa c2
  serializeWith2 pt pa (Lam cc s)         = putWord8 5 >> serializeWith pt cc >> serializeScope3 serialize (serializeWith2 pt) pa s
  serializeWith2 pt pa (Let ss s)         = putWord8 6 >> serializeWith (serializeScope3 serialize (serializeWith2 pt) pa) ss >> serializeScope3 serialize (serializeWith2 pt) pa s
  serializeWith2 pt pa (Case c bs d)      = putWord8 7 >> serializeWith2 pt pa c >> serializeWith (serializeMatch pt (serializeWith2 pt) pa) bs >> serializeWith (serializeScope3 serialize (serializeWith2 pt) pa) d
  serializeWith2 pt pa (Dict sups slts)   = putWord8 8 >> serializeWith (serializeWith2 pt pa) sups >> serializeWith (serializeScope3 serialize (serializeWith2 pt) pa) slts

  deserializeWith2 gt ga = getWord8 >>= \b -> case b of
    0 -> liftM Var ga
    1 -> liftM HardCore deserialize
    2 -> liftM4 Data (deserializeWith gt) deserialize deserialize (deserializeWith $ deserializeWith2 gt ga)
    3 -> liftM4 Prim (deserializeWith gt) gt deserialize (deserializeWith $ deserializeWith2 gt ga)
    4 -> liftM3 App gt (deserializeWith2 gt ga) (deserializeWith2 gt ga)
    5 -> liftM2 Lam (deserializeWith gt) (deserializeScope3 deserialize (deserializeWith2 gt) ga)
    6 -> liftM2 Let (deserializeWith (deserializeScope3 deserialize (deserializeWith2 gt) ga)) (deserializeScope3 deserialize (deserializeWith2 gt) ga)
    7 -> liftM3 Case (deserializeWith2 gt ga) (deserializeWith $ deserializeMatch gt (deserializeWith2 gt) ga) (deserializeWith $ deserializeScope3 deserialize (deserializeWith2 gt) ga)
    8 -> liftM2 Dict (deserializeWith (deserializeWith2 gt ga)) (deserializeWith $ deserializeScope3 deserialize (deserializeWith2 gt) ga)
    _ -> fail $ "deserializeWith: Unexpected constructor code: " ++ show b

instance Serial t => Serial1 (Core t) where
  serializeWith   = serializeWith2 serialize
  deserializeWith = deserializeWith2 deserialize

instance (Serial t, Serial a) => Serial (Core t a) where
  serialize = serializeWith serialize
  deserialize = deserializeWith deserialize

instance (Binary t, Binary a) => Binary (Core t a) where
  put = serializeWith2 Binary.put Binary.put
  get = deserializeWith2 Binary.get Binary.get

instance (Serialize t, Serialize a) => Serialize (Core t a) where
  put = serializeWith2 Serialize.put Serialize.put
  get = deserializeWith2 Serialize.get Serialize.get

instance Hashable2 Core
instance Hashable a => Hashable1 (Core a)

-- | Distinct primes used for salting the hash.
distHardCore, distData, distPrim, distApp, distLam, distLet, distCase, distDict :: Word
distHardCore = maxBound `quot` 3
distData     = maxBound `quot` 5
distPrim     = maxBound `quot` 7
distApp      = maxBound `quot` 11
distLam      = maxBound `quot` 13
distLet      = maxBound `quot` 17
distCase     = maxBound `quot` 19
distDict     = maxBound `quot` 23

instance (Hashable t, Hashable a) => Hashable (Core t a) where
  hashWithSalt n (Var a)             = hashWithSalt n a
  hashWithSalt n (HardCore c)        = hashWithSalt n c                                                      `hashWithSalt` distHardCore
  hashWithSalt n (Data cc i g cs)    = hashWithSalt n cc `hashWithSalt` i `hashWithSalt` g `hashWithSalt` cs `hashWithSalt` distData
  hashWithSalt n (Prim cc r g cs)    = hashWithSalt n cc `hashWithSalt` r `hashWithSalt` g `hashWithSalt` cs `hashWithSalt` distPrim
  hashWithSalt n (App cc x y)        = hashWithSalt n cc `hashWithSalt` x `hashWithSalt` y                   `hashWithSalt` distApp
  hashWithSalt n (Lam cc b)          = hashWithSalt n cc `hashWithSalt` b                                    `hashWithSalt` distLam
  hashWithSalt n (Let ts b)          = hashWithSalt n ts `hashWithSalt` b                                    `hashWithSalt` distLet
  hashWithSalt n (Case c bs d)       = hashWithSalt n c  `hashWithSalt` bs `hashWithSalt` d                  `hashWithSalt` distCase
  hashWithSalt n (Dict s ss)         = hashWithSalt n s  `hashWithSalt` ss                                   `hashWithSalt` distDict

instance IsString a => IsString (Core b a) where
  fromString = Var . fromString

instance AsConvention b => App (Core b) where
  _App = _AppConvention C

instance Variable (Core b) where
  _Var = prism Var $ \t -> case t of
    Var a -> Right a
    _     -> Left t
  {-# INLINE _Var #-}

instance Applicative (Core b) where
  pure = Var
  (<*>) = ap

instance Monad (Core b) where
  Var a            >>= f = f a
  HardCore h       >>= _ = HardCore h
  Data cc tg g xs  >>= f = Data cc tg g ((>>= f) <$> xs)
  Prim cc r g xs   >>= f = Prim cc r g ((>>= f) <$> xs)
  App cc x y       >>= f = App cc (x >>= f) (y >>= f)
  Lam cc e         >>= f = Lam cc (boundBy f e)
  Let bs e         >>= f = Let (boundBy f <$> bs) (boundBy f e)
  Case e as d      >>= f = Case (e >>= f) (over matchBody (boundBy f) <$> as) ((>>>= f) <$> d)
  Dict xs ys       >>= f = Dict ((>>= f) <$> xs) ((>>>= f) <$> ys)

instance Bifunctor Core where
  bimap f g = runIdentity . bitraverse (pure . f) (pure . g)

instance Bifoldable Core where
  bifoldMap = bifoldMapDefault

instance Bitraversable Core where
  bitraverse
    :: forall cc f cc' c c'. Applicative f
    => (cc -> f cc') -> (c -> f c') -> Core cc c -> f (Core cc' c')
  bitraverse f g = go
   where
   bts :: forall b. Scope b (Core cc) c -> f (Scope b (Core cc') c')
   bts (Scope e) = Scope <$> bitraverse f (traverse $ bitraverse f g) e

   go (Var a) = Var <$> g a
   go (HardCore h) = pure $ HardCore h
   go (Data cc tgl gl xs) = (Data ?? tgl ?? gl) <$> traverse f cc <*> traverse go xs
   go (Prim cc r gl xs) =
     Prim <$> traverse f cc <*> f r <*> pure gl <*> traverse go xs
   go (App cc h x) = App <$> f cc <*> go h <*> go x
   go (Lam cc e) = Lam <$> traverse f cc <*> bts e
   go (Let bs e) = Let <$> traverse bts bs <*> bts e
   go (Case e as d) =
     Case <$> go e
          <*> traverse (triverseMatch f (bitraverse f) g) as
          <*> traverse bts d
   go (Dict xs ys) = Dict <$> traverse go xs <*> traverse bts ys

instance Eq2 Core
instance Show2 Core
instance Eq b => Eq1 (Core b)
instance Show b => Show1 (Core b)

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
lam :: (Cored t m, Eq a) => t -> [a] -> Core t a -> m a
lam c as t = core $ Lam (c <$ as) (abstract (fmap fromIntegral . flip List.elemIndex as) t)

-- | Smart 'Let' constructor
let_ :: (Cored t m, Eq a) => [(a, Core t a)] -> Core t a -> m a
let_ bs b = core $ Let (abstr . snd <$> bs) (abstr b)
  where vs  = fst <$> bs
        abstr = abstract (fmap fromIntegral . flip List.elemIndex vs)

-- | Builds an n-ary data constructor
dataCon :: Cored t m => [t] -> Word64 -> Global -> m a
dataCon [] tg g = core $ Data [] tg g []
dataCon cc tg g = core $ Lam cc $ Scope $ Data cc tg g $ pure.B <$> [0..fromIntegral (length cc-1)]

-- | Builds an n-ary primop
prim :: Cored t m => [t] -> t -> Global -> m a
prim [] r g = core $ Prim [] r g []
prim cc r g = core $ Lam cc $ Scope $ Prim cc r g $ pure.B <$> [0..fromIntegral (length cc-1)]

----------------------------------------------------------------------------
-- Orphan Instances
----------------------------------------------------------------------------

instance (Hashable a, Hashable b) => Hashable (Map a b) where
  hashWithSalt n m = hashWithSalt n (Data.Map.toAscList m)
instance (Hashable a) => Hashable1 (Map a) where
  liftHashWithSalt h n m = liftHashWithSalt (liftHashWithSalt h) n (Data.Map.toAscList m)
