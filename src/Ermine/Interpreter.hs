{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

--------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett and Dan Doel 2014
-- License   :  BSD3
-- Maintainer:  Dan Doel <dan.doel@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------

module Ermine.Interpreter
  ( Address
  , Closure(..)
  , Env(..)
  , Frame(..)
  , MachineState(..)
  , eval
  , defaultMachineState
  ) where

import Control.Applicative hiding (empty)
import Control.Monad.Primitive
import Control.Monad.State
import Control.Lens hiding (au)
import qualified Data.Map as Map
import qualified Data.Foldable as F
import Data.Maybe
import Data.Primitive.MutVar
import Data.Vector (Vector)
import qualified Data.Vector as B
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Mutable as BM
import qualified Data.Vector.Primitive as P
import qualified Data.Vector.Primitive.Mutable as PM
import Data.Word
import Ermine.Syntax.G
import Ermine.Syntax.Sort
import GHC.Prim (Any)
import Prelude

newtype Address m = Address (MutVar (PrimState m) (Closure m))
  deriving Eq

-- data Value m = Addr (Address m) | Prim Word64

data Env m = Env
  { _envB :: Vector (Address m)
  , _envU :: P.Vector Word64
  , _envN :: Vector Any
  }


data Closure m
  = Closure
    { _closureCode :: !LambdaForm
    , _closureEnv  :: !(Env m)
    }
  | PartialApplication
    { _closureCode :: !LambdaForm
    , _closureEnv  :: !(Env m)
    , _papArity    :: !(Sorted Int) -- remaining args
    }
  | BlackHole

data Frame m
  = Branch !Continuation !(Env m)
  | Update !(Address m)

data MachineState m = MachineState
  { _sp      :: !(Sorted Int)
  , _fp      :: !(Sorted Int)
  , _stackF  :: [(Sorted Int, Frame m)]
  , _globalB :: Vector (Address m)
  , _stackB  :: BM.MVector (PrimState m) (Address m)
  , _stackU  :: PM.MVector (PrimState m) Word64
  , _stackN  :: BM.MVector (PrimState m) Any
  }

makeClassy ''Env
makeLenses ''MachineState

nargs :: MachineState m -> Sorted Int
nargs ms = ms^.fp -ms^.sp

sentinel :: a
sentinel = error "PANIC: access past end of stack"

defaultMachineState :: (Applicative m, PrimMonad m) => Int -> Vector (Address m) -> m (MachineState m)
defaultMachineState stackSize genv
    = MachineState (pure stackSize-1) (pure stackSize-1) [] genv
  <$> GM.replicate stackSize sentinel
  <*> GM.replicate stackSize 0
  <*> GM.replicate stackSize sentinel

poke :: PrimMonad m => Address m -> Closure m -> m ()
poke (Address r) c = writeMutVar r c

peek :: PrimMonad m => Address m -> m (Closure m)
peek (Address r) = readMutVar r

resolveEnv
  :: (Applicative m, PrimMonad m)
  => Env m -> Sorted (Vector Ref) -> MachineState m -> m (Env m)
resolveEnv le (Sorted bs us ns) ms =
  Env <$> G.mapM (resolveClosure le ms) bs
      <*> (G.convert <$> G.mapM (resolveUnboxed le ms) us)
      <*> G.mapM (resolveNative le ms) ns

resolveClosure :: PrimMonad m => Env m -> MachineState m -> Ref -> m (Address m)
resolveClosure le _ (Local l)  = return $ le^?!envB.ix (fromIntegral l)
resolveClosure _ ms (Stack s)  = GM.read (ms^.stackB) (fromIntegral s + ms^.sp.sort B)
resolveClosure _ ms (Global g) = return $ ms^?!globalB.ix (fromIntegral g)

resolveUnboxed :: PrimMonad m => Env m -> MachineState m -> Ref -> m Word64
resolveUnboxed le _ (Local l)  = return $ le^?!envU.ix (fromIntegral l)
resolveUnboxed _ ms (Stack s)  = GM.read (ms^.stackU) (fromIntegral s + ms^.sp.sort U)
resolveUnboxed _ _  _          = error "resolveEnv: Global unboxed value"

resolveNative :: PrimMonad m => Env m -> MachineState m -> Ref -> m Any
resolveNative le _ (Local l) = return $ le^?!envN.ix (fromIntegral l)
resolveNative _ ms (Stack s) = GM.read (ms^.stackN) (fromIntegral s + ms^.sp.sort N)
resolveNative _ _ _          = error "resolveEnv: Global native value"

buildClosure :: (PrimMonad m, Applicative m) => Env m -> PreClosure -> MachineState m -> m (Closure m)
buildClosure le (PreClosure captures code) ms = Closure code <$> resolveEnv le captures ms

allocClosure
  :: (Applicative m, PrimMonad m)
  => Env m -> PreClosure -> MachineState m -> m (Address m)
allocClosure le cc ms = Address <$> (buildClosure le cc ms >>= newMutVar)

allocRecursive
  :: (Applicative m, PrimMonad m)
  => Env m -> Vector PreClosure -> MachineState m -> m (MachineState m)
allocRecursive lo ccs ms = do
  let sb = ms^.sp.sort B
  let stk = ms^.stackB
  let n = B.length ccs
  ifor_ ccs $ \ i _ -> do
    mv <- newMutVar (error "PANIC: allocRecursive: closure isn't")
    GM.write stk (sb + i) (Address mv)
  let ms' = ms & sp.sort B +~ n
  ifor_ ccs $ \ i pc -> do
    addr <- GM.read stk (sb + i)
    buildClosure lo pc ms' >>= poke addr
  return ms'

pushArgs
  :: (Applicative m, PrimMonad m)
  => Env m -> Sorted (Vector Ref) -> MachineState m -> m (MachineState m)
pushArgs le refs@(Sorted bs us ns) ms = do
  let (Sorted sb su sn, ms') = ms & sp <+~ fmap B.length refs
      stkB = ms^.stackB
      stkU = ms^.stackU
      stkN = ms^.stackN
  ifor_ bs $ \i -> resolveClosure le ms >=> GM.write stkB (sb - i)
  ifor_ us $ \i -> resolveUnboxed le ms >=> GM.write stkU (su - i)
  ifor_ ns $ \i -> resolveNative  le ms >=> GM.write stkN (sn - i)
  return $ ms'

push :: Frame m -> MachineState m -> MachineState m
push fr ms = ms & fp .~ ms^.sp & stackF %~ ((ms^.fp,fr):)

copyArgs :: (PrimMonad m, GM.MVector v a) => v (PrimState m) a -> Int -> Int -> Int -> m ()
copyArgs stk frm off len = GM.move (GM.unsafeSlice (frm+off-len) len stk) (GM.unsafeSlice frm len stk)

pop :: PrimMonad m => MachineState m -> m (Maybe (Frame m, MachineState m))
pop = popWith 0

popWith :: PrimMonad m => Sorted Int -> MachineState m -> m (Maybe (Frame m, MachineState m))
popWith args ms = case ms^.stackF of
  (ofp,fr):fs -> do
    let f = ms^.fp
    ms' <- squash (f - ms^.sp) args ms
    return $ Just (fr,ms' & sp .~ f & fp .~ ofp & stackF .~ fs)
  [] -> return Nothing

-- for tail calls or return
squash :: PrimMonad m => Sorted Int -> Sorted Int -> MachineState m -> m (MachineState m)
squash sz@(Sorted zb zu zn)  args@(Sorted ab au an) ms = do
  let Sorted sb su sn = ms^.sp
      stkB = ms^.stackB
      stkU = ms^.stackU
      stkN = ms^.stackN
  copyArgs stkB sb zb ab
  copyArgs stkU su zu au
  copyArgs stkN sn zn an
  GM.set (GM.unsafeSlice sb (zb-ab) stkB) sentinel
  GM.set (GM.unsafeSlice sn (zn-an) stkN) sentinel
  return $ ms & sp +~ sz - args

select :: Continuation -> Tag -> G
select (Continuation bs df) t =
  fromMaybe (error "PANIC: missing default case in branch") $
    snd <$> Map.lookup t bs <|> df

eval :: (Applicative m, PrimMonad m) => G -> Env m -> MachineState m -> m ()
eval (App sz f xs) le ms = case f of
  Ref r -> do
    a <- resolveClosure le ms r
    ms'  <- pushArgs le xs ms
    ms'' <- squash (fromIntegral <$> sz) (G.length <$> xs) ms'
    enter a ms''
  Con t -> pushArgs le xs ms >>= returnCon t (G.length <$> xs)
eval (Let bs e) le ms = do
  let stk = ms^.stackB
      sb = ms^.sp.sort B
      ms' = ms & sp.sort B +~ G.length bs
  ifor_ bs $ \i pc -> allocClosure le pc ms' >>= GM.write stk (sb + i)
  eval e le ms'
eval (LetRec bs e) le ms   = allocRecursive le bs ms >>= eval e le
eval (Case co k) le ms     = eval co le $ push (Branch k le) ms
eval (CaseLit ref k) le ms = do
  l <- resolveUnboxed le ms ref
  returnLit l $ push (Branch k le) ms
eval (Lit l) _ ms = returnLit l ms

extendPayload :: (PrimMonad m, G.Vector v a) => v a -> G.Mutable v (PrimState m) a -> Int -> Int -> m (v a)
extendPayload d stk stp n = do
  let m = G.length d
  me <- GM.new (m + n)
  G.unsafeCopy (GM.unsafeSlice 0 m me) d
  GM.unsafeCopy (GM.unsafeSlice m n me) (GM.unsafeSlice stp n stk)
  G.unsafeFreeze me

pushPayload :: (PrimMonad m, G.Vector v a) => Int -> v a -> G.Mutable v (PrimState m) a -> Int -> m ()
pushPayload f e stk stp = G.copy (GM.slice stp (G.length e - f) stk) (G.unsafeDrop f e)

pushPayloads :: PrimMonad m => Sorted Int -> Env m -> MachineState m -> m (MachineState m)
pushPayloads (Sorted fb fu fn) (Env db du dn) ms = do
  let Sorted sb su sn = ms^.sp
  pushPayload fb db (ms^.stackB) sb
  pushPayload fu du (ms^.stackU) su
  pushPayload fn dn (ms^.stackN) sn
  return $ ms & sp +~ Sorted (G.length db - fb) (G.length du - fu) (G.length dn - fn)

extendPayloads :: (Applicative m, PrimMonad m) => Env m -> MachineState m -> m (Env m)
extendPayloads (Env db du dn) ms =
  Env <$> extendPayload db (ms^.stackB) sb arb
      <*> extendPayload du (ms^.stackU) su aru
      <*> extendPayload dn (ms^.stackN) sn arn
  where
    Sorted sb su sn = ms^.sp
    Sorted arb aru arn = nargs ms

enter :: (Applicative m, PrimMonad m) => Address m -> MachineState m -> m ()
enter addr ms = peek addr >>= \case
  BlackHole -> fail "ermine <<loop>> detected"
  PartialApplication co da arity
    | F.sum args < F.sum arity -> pap co da arity
    | otherwise -> pushPayloads (fmap fromIntegral (co^.free)) da ms >>= eval (co^.body) da
  Closure co da
    | co^.update -> eval (co^.body) da $ push (Update addr) ms
    | arity <- fmap fromIntegral (co^.bound), F.sum args < F.sum arity -> pap co da arity
    | otherwise -> eval (co^.body) da ms
 where
  args = nargs ms
  pap co da arity = do
    e <- extendPayloads da ms
    pop ms >>= \case
      Just (Update ad, ms') -> do
        poke ad $ PartialApplication co e (arity - args)
        enter ad ms'
      Just (Branch (Continuation m (Just dflt)) le', ms') | Map.null m -> eval dflt le' ms'
      Just _  -> error "bad frame after partial application"
      Nothing -> return ()

payload :: (PrimMonad m, G.Vector v a) => G.Mutable v (PrimState m) a -> Int -> Int -> m (v a)
payload mv s n = do
  me <- GM.new n
  GM.unsafeCopy me (GM.unsafeSlice s n mv)
  G.unsafeFreeze me

returnCon :: (Applicative m, PrimMonad m) => Tag -> Sorted Int -> MachineState m -> m ()
returnCon t args@(Sorted ab au an) ms = popWith args ms >>= \case
  Just (Branch k le, ms') -> eval (select k t) le ms'
  Just (Update ad, ms') -> do
    let Sorted sb su sn = ms'^.sp
    e <- Env <$> payload (ms^.stackB) sb ab <*> payload (ms^.stackU) su au <*> payload (ms^.stackN) sn an
    poke ad $ Closure (standardConstructor (fromIntegral <$> args) t) e
    returnCon t args ms'
  Nothing -> return ()

returnLit :: (Applicative m, PrimMonad m) => Word64 -> MachineState m -> m ()
returnLit w ms = pop ms >>= \case
  Just (Branch k le, ms') -> eval (select k w) le ms'
  Just _ -> error "PANIC: literal update frame"
  Nothing -> return ()
