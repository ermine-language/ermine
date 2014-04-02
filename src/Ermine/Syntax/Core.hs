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
  , Lit(..)
  , super
  , slot
  -- * Smart constructors
  , lam
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
import Ermine.Syntax.Convention
import Ermine.Syntax.Head
import Ermine.Syntax.Global as Global hiding (N)
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
-- Data [C, C] 1
--   (glob (Infix R 5) (mkModuleName "ermine" "Prelude") "(:)")
--   [Data [U] 0
--      (glob Idfix (mkModuleName "ermine" "Prelude") "Literal")
--      [HardCore (Lit (Int 1))],
--    Data [] 0 (glob Idfix (mkModuleName "ermine" "Prelude") "[]") []]
instance Cons (Core a) (Core a) (Core a) (Core a) where
  _Cons = prism (\(a, as) -> Data [C,C] 1 consg [a,as]) $ \ s -> case s of
    Data [C,C] 1 _ [x,xs] -> Right (x,xs)
    _                     -> Left s

-- | The built-in '[]' constructor for a list.
nil :: Core a
nil = Data [] 0 nilg []

-- | The built-in 'Just' constructor for 'Maybe'.
just :: Core a -> Core a
just a = Data [C] 1 justg [a]

-- | The built-in 'Nothing' constructor for 'Maybe'.
nothing :: Core a
nothing = Data [] 0 nothingg []

-- | Lifting of literal values to core.
class Lit a where
  lit  :: a   -> Core b
  lits :: [a] -> Core b
  lits = Prelude.foldr (Lens.cons . lit) nil

instance Lit Int64 where
  lit l = Data [U] 0 literalg [HardCore $ Lit $ Long l]

instance Lit Int32 where
  lit i = Data [U] 0 literalg [HardCore $ Lit $ Int i]

instance Lit Char where
  lit c  = Data [U] 0 literalg [HardCore $ Lit $ Char c]
  lits s = Data [N] 0 stringg [HardCore $ Lit $ String (Strict.pack s)]

instance Lit Int8 where
  lit b = Data [U] 0 literalg [HardCore $ Lit $ Byte b]

instance Lit Int16 where
  lit s = Data [U] 0 literalg [HardCore $ Lit $ Short s]

instance (Lit a, Lit b) => Lit (a, b) where
  lit (a,b) = Data [C,C] 0 (tupleg 2) [lit a, lit b]

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
class (Variable c, AppHash c, AppDict c, App c, Applicative c, Monad c) => Cored c where
  core :: Core a -> c a
  case_ :: c a -> Map Word8 (Match c a) -> Maybe (Scope () c a) -> c a
  caseLit :: Bool -> c a -> Map Literal (c a) -> Maybe (c a) -> c a
  lambda :: [Convention] -> Convention -> Scope Word8 c a -> c a
  letrec :: [Scope Word32 c a] -> Scope Word32 c a -> c a
  hardCore :: HardCore -> c a
  hardCore = core . HardCore
  {-# INLINE hardCore #-}

instance Cored Core where
  core = id
  {-# INLINE core #-}
  case_ = Case
  caseLit = CaseLit
  lambda [] _ s = instantiate (error "lambda: impossible argument") s
  lambda cc r s = Lam cc r s
  letrec = Let

instance Cored m => Cored (Scope b m) where
  core = lift . core
  {-# INLINE core #-}
  case_ e bs d = Scope $ case_ (unscope e) (fmap (over matchBody expandScope) bs) (fmap expandScope d)
  caseLit n e bs d = Scope $ caseLit n (unscope e) (unscope <$> bs) (unscope <$> d)
  lambda cc r e = Scope . lambda cc r $ expandScope e
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

-- | 'Core' values are the output of the compilation process.
--
-- They are terms where the dictionary passing has been made explicit
-- and all of the types have been checked and removed.
data Core a
  = Var a
  | HardCore !HardCore
  | Data [Convention] !Word8 !Global [Core a] -- convention, tag #, associated global for display purposes, cores
  | App !Convention !(Core a) !(Core a)
  | Lam [Convention] Convention !(Scope Word8 Core a)
  | Let [Scope Word32 Core a] !(Scope Word32 Core a)
  | Case !(Core a) (Map Word8 (Match Core a)) (Maybe (Scope () Core a))
  | Dict { supers :: [Core a], slots :: [Scope Word8 Core a] }
  | CaseLit !Bool !(Core a) (Map Literal (Core a)) (Maybe (Core a)) -- set True for native for strings
  deriving (Eq,Show,Functor,Foldable,Traversable)

data Match c a = Match
  { _matchArgs   :: [Convention]
  , _matchGlobal :: !Global
  , _matchBody   :: Scope Word8 c a
  } deriving (Eq,Show,Functor,Foldable,Traversable)

matchArgs :: Lens' (Match c a) [Convention]
matchArgs f (Match a g b) = f a <&> \a' -> Match a' g b

matchGlobal :: Lens' (Match c a) Global
matchGlobal f (Match a g b) = f g <&> \g' -> Match a g' b

matchBody :: Lens (Match c a) (Match d b) (Scope Word8 c a) (Scope Word8 d b)
matchBody f (Match a g b) = Match a g <$> f b

instance AsHardCore (Core a) where
  _HardCore = prism HardCore $ \c -> case c of
    HardCore hc -> Right hc
    _           -> Left c

-- | ask for the @n@th the superclass of a given dictionary as a core expression
super :: (AsHardCore (c a), AppDict c) => Word8 -> c a -> c a
super i c = _AppDict # (_Super # i, c)

-- | ask for the @n@th slot of a given dictionary as a core expression
slot :: (AsHardCore (c a), AppDict c) => Word8 -> c a -> c a
slot i c = _AppDict # (_Slot # i, c)

instance AppDict Core where
  _AppDict = prism (uncurry $ App D) $ \ xs -> case xs of App D f d -> Right (f, d) ; c -> Left c

instance AppHash Core where
  _AppHash = prism (uncurry $ App U) $ \ xs -> case xs of App U f d -> Right (f, d) ; c -> Left c

instance (Hashable a, Hashable b) => Hashable (Map a b) where
  hashWithSalt n m = hashWithSalt n (Data.Map.toAscList m)

instance Serial1 Core where
  -- | Binary serialization of a 'Core', given serializers for its parameter.
  serializeWith pa (Var a)            = putWord8 0 >> pa a
  serializeWith _  (HardCore h)       = putWord8 1 >> serialize h
  serializeWith pa (Data cc i g cs)   = putWord8 2 >> serialize cc >> serialize i >> serialize g >> serializeWith (serializeWith pa) cs
  serializeWith pa (App cc c1 c2)     = putWord8 3 >> serialize cc >> serializeWith pa c1 >> serializeWith pa c2
  serializeWith pa (Lam cc r s)       = putWord8 4 >> serialize cc >> serialize r >> serializeWith pa s
  serializeWith pa (Let ss s)         = putWord8 5 >> serializeWith (serializeWith pa) ss >> serializeWith pa s
  serializeWith pa (Case c bs d)      = putWord8 6 >> serializeWith pa c >> serializeWith (serializeWith pa) bs >> serializeWith (serializeWith pa) d
  serializeWith pa (Dict sups slts)   = putWord8 7 >> serializeWith (serializeWith pa) sups >> serializeWith (serializeWith pa) slts
  serializeWith pa (CaseLit n c bs d) = putWord8 8 >> serialize n >> serializeWith pa c >> serializeWith (serializeWith pa) bs >> serializeWith (serializeWith pa) d

  deserializeWith ga = getWord8 >>= \b -> case b of
    0 -> liftM Var ga
    1 -> liftM HardCore deserialize
    2 -> liftM4 Data deserialize deserialize deserialize (deserializeWith $ deserializeWith ga)
    3 -> liftM3 App deserialize (deserializeWith ga) (deserializeWith ga)
    4 -> liftM3 Lam deserialize deserialize (deserializeWith ga)
    5 -> liftM2 Let (deserializeWith (deserializeWith ga)) (deserializeWith ga)
    6 -> liftM3 Case (deserializeWith ga) (deserializeWith $ deserializeWith ga) (deserializeWith $ deserializeWith ga)
    7 -> liftM2 Dict (deserializeWith (deserializeWith ga)) (deserializeWith $ deserializeWith ga)
    8 -> liftM4 CaseLit deserialize (deserializeWith ga) (deserializeWith $ deserializeWith ga) (deserializeWith $ deserializeWith ga)
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
distHardCore, distData, distApp, distLam, distLet, distCase, distDict, distCaseLit :: Word
distHardCore = maxBound `quot` 3
distData     = maxBound `quot` 5
distApp      = maxBound `quot` 7
distLam      = maxBound `quot` 11
distLet      = maxBound `quot` 13
distCase     = maxBound `quot` 17
distDict     = maxBound `quot` 19
distCaseLit  = maxBound `quot` 23

instance Hashable a => Hashable (Core a) where
  hashWithSalt n (Var a)             = hashWithSalt n a
  hashWithSalt n (HardCore c)        = hashWithSalt n c                                                      `hashWithSalt` distHardCore
  hashWithSalt n (Data cc i g cs)    = hashWithSalt n cc `hashWithSalt` i `hashWithSalt` g `hashWithSalt` cs `hashWithSalt` distData
  hashWithSalt n (App cc x y)        = hashWithSalt n cc `hashWithSalt` x `hashWithSalt` y                   `hashWithSalt` distApp
  hashWithSalt n (Lam cc r b)        = hashWithSalt n cc `hashWithSalt` r `hashWithSalt` b                   `hashWithSalt` distLam
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
  App cc x y       >>= f = App cc (x >>= f) (y >>= f)
  Lam cc r e       >>= f = Lam cc r (boundBy f e)
  Let bs e         >>= f = Let (boundBy f <$> bs) (boundBy f e)
  Case e as d      >>= f = Case (e >>= f) (over matchBody (boundBy f) <$> as) ((>>>= f) <$> d)
  Dict xs ys       >>= f = Dict ((>>= f) <$> xs) ((>>>= f) <$> ys)
  CaseLit c e as d >>= f = CaseLit c (e >>= f) ((>>= f) <$> as) ((>>= f) <$> d)

instance Eq1 Core
instance Show1 Core

-- | Smart 'Lam' constructor
lam :: (Cored m, Eq a) => Convention -> Convention -> [a] -> Core a -> m a
lam c r as t = core $ Lam (c <$ as) r (abstract (fmap fromIntegral . flip List.elemIndex as) t)

-- | Smart 'Let' constructor
let_ :: (Cored m, Eq a) => [(a, Core a)] -> Core a -> m a
let_ bs b = core $ Let (abstr . snd <$> bs) (abstr b)
  where vs  = fst <$> bs
        abstr = abstract (fmap fromIntegral . flip List.elemIndex vs)

-- | Builds an n-ary data constructor
dataCon :: Cored m => [Convention] -> Word8 -> Global -> m a
dataCon [] tg g = core $ Data [] tg g []
dataCon cc tg g = core $ Lam cc C $ Scope $ Data cc tg g $ pure.B <$> [0..fromIntegral (length cc-1)]

-- * Match instances

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
