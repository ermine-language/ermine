{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett 2012
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
  , Cored(..)
  , AsAppDict(..)
  , appDicts
  , AsAppHash(..)
  , appHashes
  , JavaLike(..)
  , Foreign(..)
  , HardCore(..)
  , AsHardCore(..)
  , Lit(..)
  , super
  , slot
  -- * Smart constructors
  , lam
  , lamHash
  , let_
  , dataCon
  -- * Common built-in terms
  , cons
  , nil
  , just, nothing
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
import Data.Int
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
import Ermine.Syntax.Head
import Ermine.Syntax.Global as Global
import Ermine.Syntax.Literal
import Ermine.Syntax.Scope
import GHC.Generics
import Prelude.Extras
import Prelude

-- $setup
-- >>> import Text.Groom

-- | The built-in '::' constructor for a list.
--
-- >>> putStrLn $ groom $ lit (1 :: Int32) `cons` nil
-- Data 1 (glob (Infix R 5) (mkModuleName "ermine" "Builtin") "(:)")
--   [HardCore (Lit (Int 1)),
--    Data 0 (glob Idfix (mkModuleName "ermine" "Builtin") "[]") []]
instance Cons (Core a) (Core a) (Core a) (Core a) where
  _Cons = prism (\(a, as) -> Data 1 consg [a,as]) $ \ s -> case s of
    Data 1 _ [x,xs] -> Right (x,xs)
    _               -> Left s

-- | The built-in '[]' constructor for a list.
nil :: Core a
nil = Data 0 nilg []

-- | The built-in 'Just' constructor for 'Maybe'.
just :: Core a -> Core a
just a = Data 1 justg [a]

-- | The built-in 'Nothing' constructor for 'Maybe'.
nothing :: Core a
nothing = Data 0 nothingg []

-- | Lifting of literal values to core.
class Lit a where
  lit  :: a   -> Core b
  lits :: [a] -> Core b
  lits = Prelude.foldr (Lens.cons . lit) nil

instance Lit Int64 where lit l = HardCore . Lit $ Long l
instance Lit Int32 where lit i = HardCore . Lit $ Int i
instance Lit Char where
  lit c = HardCore . Lit $ Char c
  lits s = HardCore . Lit $ String (Strict.pack s)
instance Lit Int8 where lit b = HardCore . Lit $ Byte b
instance Lit Int16 where lit s = HardCore . Lit $ Short s
instance (Lit a, Lit b) => Lit (a, b) where
  lit (a,b) = Data 0 (tupleg 2) [lit a, lit b]
instance Lit a => Lit [a] where
  lit = lits
instance Lit a => Lit (Maybe a) where
  lit = maybe nothing (just . lit)

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

-- | 'HardCore' is the subset of 'Core' terms that can be unified with value equality.
data HardCore
  = Super      !Word8
  | Slot       !Word8
  | Lit        !Literal
  | PrimOp     !Strict.Text
  | Foreign    !Foreign
  | Error      !Strict.Text
  | GlobalId   !Global
  | InstanceId !Head
  deriving (Eq,Ord,Show,Read,Data,Typeable,Generic)

-- | This class describes things that could be 'HardCore'.
class AsHardCore c where
  _HardCore :: Prism' c HardCore

  _Lit :: Prism' c Literal
  _Lit = _HardCore._Lit

  _PrimOp :: Prism' c Strict.Text
  _PrimOp = _HardCore._PrimOp

  _Error :: Prism' c Strict.Text
  _Error = _HardCore._Error

  _Super :: Prism' c Word8
  _Super = _HardCore._Super

  _Slot  :: Prism' c Word8
  _Slot = _HardCore._Slot

  _Foreign :: Prism' c Foreign
  _Foreign = _HardCore._Foreign

  _GlobalId :: Prism' c Global
  _GlobalId = _HardCore._GlobalId

  _InstanceId :: Prism' c Head
  _InstanceId = _HardCore._InstanceId

instance f ~ Core => AsHardCore (Scope b f a) where
  _HardCore = prism (Scope . HardCore) $ \ t@(Scope b) -> case b of
    HardCore k           -> Right k
    Var (F (HardCore k)) -> Right k
    _                    -> Left t

instance AsHardCore HardCore where
  _HardCore = id

  _Lit        = prism Lit        $ \ xs -> case xs of Lit     l -> Right l ; hc -> Left hc
  _PrimOp     = prism PrimOp     $ \ xs -> case xs of PrimOp  l -> Right l ; hc -> Left hc
  _Error      = prism Error      $ \ xs -> case xs of Error   l -> Right l ; hc -> Left hc
  _Super      = prism Super      $ \ xs -> case xs of Super   l -> Right l ; hc -> Left hc
  _Slot       = prism Slot       $ \ xs -> case xs of Slot    l -> Right l ; hc -> Left hc
  _Foreign    = prism Foreign    $ \ xs -> case xs of Foreign l -> Right l ; hc -> Left hc
  _GlobalId   = prism GlobalId   $ \ xs -> case xs of GlobalId l -> Right l; hc -> Left hc
  _InstanceId = prism InstanceId $ \ xs -> case xs of InstanceId l -> Right l; hc -> Left hc

instance Hashable HardCore

instance Serial HardCore where
  serialize (Super i)      = putWord8 0 >> serialize i
  serialize (Slot g)       = putWord8 1 >> serialize g
  serialize (Lit i)        = putWord8 2 >> serialize i
  serialize (PrimOp s)     = putWord8 3 >> serialize s
  serialize (Foreign f)    = putWord8 4 >> serialize f
  serialize (Error s)      = putWord8 5 >> serialize s
  serialize (GlobalId g)   = putWord8 6 >> serialize g
  serialize (InstanceId i) = putWord8 7 >> serialize i

  deserialize = getWord8 >>= \b -> case b of
    0 -> liftM Super      deserialize
    1 -> liftM Slot       deserialize
    2 -> liftM Lit        deserialize
    3 -> liftM PrimOp     deserialize
    4 -> liftM Foreign    deserialize
    5 -> liftM Error      deserialize
    6 -> liftM GlobalId   deserialize
    7 -> liftM InstanceId deserialize
    _ -> fail $ "get HardCore: Unexpected constructor code: " ++ show b

instance Binary HardCore where
  put = serialize
  get = deserialize

instance Serialize HardCore where
  put = serialize
  get = deserialize

-- | Instances of this are things we can both construct from a 'Core' expression,
-- and perform case analysis upon.
--
-- e.g. 'Core' and @'Scope' b 'Core'@
class (Applicative c, Monad c) => Cored c where
  core :: Core a -> c a
  caze :: c a -> Map Word8 (Word8, Global, Scope Word8 c a) -> Maybe (Scope () c a) -> c a
  lambda :: Word8 -> Scope Word8 c a -> c a
  lambdaDict :: Scope () c a -> c a
  letrec :: [Scope Word32 c a] -> Scope Word32 c a -> c a
  hardCore :: HardCore -> c a
  hardCore = core . HardCore
  {-# INLINE hardCore #-}

instance Cored Core where
  core = id
  {-# INLINE core #-}
  caze = Case
  lambda 0 s = instantiate (error "lambda: impossible argument") s
  lambda n s = Lam n s
  lambdaDict = LamDict
  letrec = Let

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

instance Cored m => Cored (Scope b m) where
  core = lift . core
  {-# INLINE core #-}
  caze e bs d = Scope $
    caze (unscope e)
         (fmap (over _3 expandScope) bs)
         (fmap expandScope d)
  lambda w e = Scope . lambda w $ expandScope e
  lambdaDict e = Scope . lambdaDict $ expandScope e
  letrec ds body = Scope $ letrec (expandScope <$> ds) (expandScope body)

-- | 'Core' values are the output of the compilation process.
--
-- They are terms where the dictionary passing has been made explicit
-- and all of the types have been checked and removed.
data Core a
  = Var a
  | HardCore !HardCore
  | Data !Word8 !Global [Core a]
  | App !(Core a) !(Core a)
  | Lam !Word8 !(Scope Word8 Core a)
  | Let [Scope Word32 Core a] !(Scope Word32 Core a)
  | Case !(Core a) (Map Word8 (Word8, Global, Scope Word8 Core a)) (Maybe (Scope () Core a))
  | Dict { supers :: [Core a], slots :: [Scope Word8 Core a] }
  | LamDict !(Scope () Core a)
  | AppDict !(Core a) !(Core a)
  | LamHash !Word8 !(Scope Word8 Core a)
  | AppHash !(Core a) !(Core a)
  deriving (Eq,Show,Functor,Foldable,Traversable)

instance AsHardCore (Core a) where
  _HardCore = prism HardCore $ \c -> case c of
    HardCore hc -> Right hc
    _           -> Left c

-- | ask for the @n@th the superclass of a given dictionary as a core expression
super :: (AsHardCore c, AsAppDict c) => Word8 -> c -> c
super i c = _AppDict # (_Super # i, c)

-- | ask for the @n@th slot of a given dictionary as a core expression
slot :: (AsHardCore c, AsAppDict c) => Word8 -> c -> c
slot i c = _AppDict # (_Slot # i, c)

class AsAppHash c where
  _AppHash :: Prism' c (c, c)

instance AsAppHash (Core a) where
  _AppHash = prism (uncurry AppHash) $ \ xs -> case xs of AppHash f d -> Right (f, d) ; c -> Left c

instance f ~ Core => AsAppHash (Scope b f a) where
  _AppHash = prism (\ (Scope x, Scope y) -> Scope $ AppHash x y) $ \t@(Scope b) -> case b of
    AppHash x y -> Right (Scope x,Scope y)
    Var (F (AppHash x y)) -> Right (Scope (Var (F x)), Scope (Var (F y)))
    _ -> Left t

-- | 'AppDict' provides strict application for dictionary manipulation
class AsAppDict c where
  _AppDict :: Prism' c (c, c)

instance AsAppDict (Core a) where
  _AppDict = prism (uncurry AppDict) $ \ xs -> case xs of AppDict f d -> Right (f, d) ; c -> Left c

instance f ~ Core => AsAppDict (Scope b f a) where
  _AppDict = prism (\ (Scope x, Scope y) -> Scope $ AppDict x y) $ \t@(Scope b) -> case b of
    AppDict x y -> Right (Scope x,Scope y)
    Var (F (AppDict x y)) -> Right (Scope (Var (F x)), Scope (Var (F y)))
    _ -> Left t

-- | Apply several dictionaries.
appDicts :: AsAppDict c => c -> [c] -> c
appDicts = Prelude.foldl (curry $ review _AppDict)

-- | Apply several dictionaries.
appHashes :: AsAppHash c => c -> [c] -> c
appHashes = Prelude.foldl (curry $ review _AppHash)

instance (Hashable a, Hashable b) => Hashable (Map a b) where
  hashWithSalt n m = hashWithSalt n (Data.Map.toAscList m)

instance Serial1 Core where
  -- | Binary serialization of a 'Core', given serializers for its parameter.
  serializeWith pa (Var a)          = putWord8 0  >> pa a
  serializeWith _  (HardCore h)     = putWord8 1  >> serialize h
  serializeWith pa (Data i g cs)    = putWord8 2  >> serialize i >> serialize g >> serializeWith (serializeWith pa) cs
  serializeWith pa (App c1 c2)      = putWord8 3  >> serializeWith pa c1 >> serializeWith pa c2
  serializeWith pa (Lam i s)        = putWord8 4  >> serialize i >> serializeWith pa s
  serializeWith pa (Let ss s)       = putWord8 5  >> serializeWith (serializeWith pa) ss >> serializeWith pa s
  serializeWith pa (Case c bs d)    = putWord8 6  >> serializeWith pa c >> serializeWith (serializeWith $ serializeWith pa) bs >> serializeWith (serializeWith pa) d
  serializeWith pa (Dict sups slts) = putWord8 7  >> serializeWith (serializeWith pa) sups >> serializeWith (serializeWith pa) slts
  serializeWith pa (LamDict s)      = putWord8 8  >> serializeWith pa s
  serializeWith pa (AppDict c1 c2)  = putWord8 9  >> serializeWith pa c1 >> serializeWith pa c2
  serializeWith pa (LamHash i s)    = putWord8 10 >> serialize i >> serializeWith pa s
  serializeWith pa (AppHash c1 c2)  = putWord8 11 >> serializeWith pa c1 >> serializeWith pa c2

  deserializeWith ga = getWord8 >>= \b -> case b of
    0  -> liftM Var ga
    1  -> liftM HardCore deserialize
    2  -> liftM3 Data deserialize deserialize (deserializeWith (deserializeWith ga))
    3  -> liftM2 App (deserializeWith ga) (deserializeWith ga)
    4  -> liftM2 Lam deserialize (deserializeWith ga)
    5  -> liftM2 Let (deserializeWith (deserializeWith ga)) (deserializeWith ga)
    6  -> liftM3 Case (deserializeWith ga) (deserializeWith (deserializeWith $ deserializeWith ga)) (deserializeWith (deserializeWith ga))
    7  -> liftM2 Dict (deserializeWith (deserializeWith ga)) (deserializeWith (deserializeWith ga))
    8  -> liftM LamDict (deserializeWith ga)
    9  -> liftM2 AppDict (deserializeWith ga) (deserializeWith ga)
    10 -> liftM2 LamHash deserialize (deserializeWith ga)
    11 -> liftM2 AppHash (deserializeWith ga) (deserializeWith ga)
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
distHardCore, distData, distApp, distLam, distLet, distCase, distDict, distLamDict, distAppDict, distLamHash, distAppHash :: Word
distHardCore = maxBound `quot` 3
distData     = maxBound `quot` 5
distApp      = maxBound `quot` 7
distLam      = maxBound `quot` 11
distLet      = maxBound `quot` 13
distCase     = maxBound `quot` 17
distDict     = maxBound `quot` 19
distLamDict  = maxBound `quot` 23
distAppDict  = maxBound `quot` 29
distLamHash  = maxBound `quot` 31
distAppHash  = maxBound `quot` 37

instance Hashable a => Hashable (Core a) where
  hashWithSalt n (Var a)       = hashWithSalt n a
  hashWithSalt n (HardCore c)  = hashWithSalt n c                                     `hashWithSalt` distHardCore
  hashWithSalt n (Data i g cs) = hashWithSalt n i  `hashWithSalt` cs `hashWithSalt` g `hashWithSalt` distData
  hashWithSalt n (App x y)     = hashWithSalt n x  `hashWithSalt` y                   `hashWithSalt` distApp
  hashWithSalt n (Lam k b)     = hashWithSalt n k  `hashWithSalt` b                   `hashWithSalt` distLam
  hashWithSalt n (Let ts b)    = hashWithSalt n ts `hashWithSalt` b                   `hashWithSalt` distLet
  hashWithSalt n (Case c bs d) = hashWithSalt n c  `hashWithSalt` bs `hashWithSalt` d `hashWithSalt` distCase
  hashWithSalt n (Dict s ss)   = hashWithSalt n s  `hashWithSalt` ss                  `hashWithSalt` distDict
  hashWithSalt n (LamDict b)   = hashWithSalt n b                                     `hashWithSalt` distLamDict
  hashWithSalt n (AppDict x y) = hashWithSalt n x  `hashWithSalt` y                   `hashWithSalt` distAppDict
  hashWithSalt n (LamHash k b) = hashWithSalt n k  `hashWithSalt` b                   `hashWithSalt` distLamHash
  hashWithSalt n (AppHash x y) = hashWithSalt n x  `hashWithSalt` y                   `hashWithSalt` distAppHash

instance IsString a => IsString (Core a) where
  fromString = Var . fromString

instance App Core where
  _App = prism (uncurry App) $ \t -> case t of
    App l r -> Right (l,r)
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
  Var a       >>= f = f a
  HardCore h  >>= _ = HardCore h
  Data n g xs >>= f = Data n g ((>>= f) <$> xs)
  App x y     >>= f = App (x >>= f) (y >>= f)
  Lam n e     >>= f = Lam n (boundBy f e)
  Let bs e    >>= f = Let (boundBy f <$> bs) (boundBy f e)
  Case e as d >>= f = Case (e >>= f) (over _3 (boundBy f) <$> as) ((>>>= f) <$> d)
  Dict xs ys  >>= f = Dict ((>>= f) <$> xs) ((>>>= f) <$> ys)
  LamDict e   >>= f = LamDict (e >>>= f)
  AppDict x y >>= f = AppDict (x >>= f) (y >>= f)
  LamHash i e >>= f = LamHash i (e >>>= f)
  AppHash x y >>= f = AppHash (x >>= f) (y >>= f)

instance Eq1 Core
instance Show1 Core

-- | Smart 'Lam' constructor
lam :: (Cored m, Eq a) => [a] -> Core a -> m a
lam as t = core $ Lam (fromIntegral $ length as)
  (abstract (fmap fromIntegral . flip List.elemIndex as) t)

-- | Smart 'Lam' constructor
lamHash :: (Cored m, Eq a) => [a] -> Core a -> m a
lamHash as t = core $ LamHash (fromIntegral $ length as)
  (abstract (fmap fromIntegral . flip List.elemIndex as) t)

-- | Smart 'Let' constructor
let_ :: (Cored m, Eq a) => [(a, Core a)] -> Core a -> m a
let_ bs b = core $ Let (abstr . snd <$> bs) (abstr b)
  where vs  = fst <$> bs
        abstr = abstract (fmap fromIntegral . flip List.elemIndex vs)

-- | Builds an n-ary data constructor
--
-- TODO: gracefully handle unboxed fields
dataCon :: Cored m => Word8 -> Word8 -> Global -> m a
dataCon 0     tg g = core $ Data tg g []
dataCon arity tg g = core $ Lam arity . Scope . Data tg g $ pure . B <$> [0 .. arity-1]
