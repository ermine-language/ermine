{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

--------------------------------------------------------------------
-- |
-- Copyright :  (c) Dan Doel 2014
-- License   :  BSD3
-- Maintainer:  Dan Doel <dan.doel@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------

module Ermine.Interpreter
  ( Address
  , Closure
  , code
  , frame
  , Value(..)
  , GlobalEnv
  , LEnv
  , Frame(..)
  , MachineState
  , args
  , stack
  , genv
  , eval
  ) where

import Control.Applicative hiding (empty)
import Control.Monad.Primitive
import Control.Monad.State
import Control.Lens
import Data.Default
import Data.Map hiding (null, update, filter)
import Data.Maybe
import Data.Vector (Vector)
import qualified Data.Vector as B
import qualified Data.Vector.Generic as G
import Data.Vector.Generic.Mutable as GM
import Data.Vector.Mutable as BM
import qualified Data.Vector.Primitive as P
import Data.Vector.Primitive.Mutable as PM
import Data.Primitive.MutVar
import Data.Traversable
import Data.Word
import Ermine.Syntax.G
import GHC.Prim (Any)
import Prelude hiding (lookup)

genericLength :: Num n => [a] -> n
genericLength = fromIntegral . length

newtype Address m = Address (MutVar (PrimState m) (Closure m))
  deriving Eq

-- data Value m = Addr (Address m) | Prim Word64

data Env m = Env
  { _envB :: Vector (Address m)
  , _envU :: P.Vector Word64
  , _envN :: Vector Any
  }

makeClassy ''Env

data Closure m
  = Closure
    { _closureCode :: !LambdaForm
    , _closureEnv  :: !(Env m)
    }
  | PartialApplication
    { _closureCode :: !LambdaForm
    , _closureEnv  :: !(Env m)
    , _papArity    :: !Sorted Int -- remaining args
    }
  | BlackHole
  | Branch Continuation

data Frame m
  = Branch
  { _frameContinuaton :: Continuation
  , _framePointer :: !(Sorted Int)
  }
  | Update
  { _frameAddress :: Address m
  , _framePointer :: !(Sorted Int)
  }

data GlobalEnv m = GlobalEnv
  { _globalB :: Vector (Address m)
  }

data MachineState m = MachineState
  { _sp    :: !(Sorted Int)
  , _fp    :: !(Sorted Int)
  , _stackB :: MVector (PrimState m) (Address m)
  , _stackU :: PM.MVector (PrimState m) Word64
  , _stackN :: MVector (PrimState m) Any
  , _stackF :: [Frame m]
  , _genv  :: GlobalEnv m -- environment
  }

poke :: PrimMonad m => Address m -> Closure m -> m ()
poke (Address r) c = writeMutVar r c

peek :: PrimMonad m => Address m -> m (Closure m)
peek (Address r) = readMutVar r

resolveEnv
  :: (Applicative m, PrimMonad m)
  => GlobalEnv m -> Env m -> MachineState m -> Sorted [Ref] -> m (Env m)
resolveEnv ge le ms (Sorted bs us ns) =
  env <$> traverse resolveClosure bs
      <*> traverse resolveUnboxed us
      <*> traverse resolveNative ns
  where
    env a b c = Env (G.fromList a) (G.fromList b) (G.fromList c)
    resolveClosure (Local l)  = return $ le^?!envB.ix l
    resolveClosure (Stack s)  = GM.read (ms^?!stackB) (s + ms^.fp.sort B)
    resolveClosure (Global g) = return $ ms^?!genv.ix g
    resolveUnboxed (Local l)  = return $ le^?!envU.ix l
    resolveUnboxed (Stack s)  = GM.read (ms^?!stackU) (s + ms^.fp.sort U)
    resolveUnboxed (Global g) = error "resolveEnv: Global unboxed value"
    resolveNative  (Local l)  = return $ le^?!envN.ix l
    resolveNative  (Stack s)  = GM.read (ms^?!stackN) (s + ms^.fp.sort N)
    resolveNative  (Global g) = error "resolveEnv: Global native value"

-- extend :: Integral k => Map k a -> [a] -> Map k a
-- extend lo vs = fromList ([w ..] `zip` vs) `union` lo
-- where w = fresh lo

buildClosure :: GlobalEnv m -> Env m -> PreClosure -> MachineState m -> m (Closure m)
buildClosure ge lo (PreClosure captures code) ms = Closure code <$> resolveEnv ge le ms captures

allocClosure
  :: (Functor m, PrimMonad m)
  => GlobalEnv m -> Env m -> PreClosure -> MachineState m -> m (Address m)
allocClosure ge le cc ms = Address <$> (newMutVar buildClosure gl lo cc ms >>= newMutVar)

-- needs a lot of updating
allocRecursive
  :: (Applicative m, PrimMonad m)
  => GlobalEnv m -> Env m -> [PreClosure] -> MachineState m -> m (Env m)
allocRecursive gl lo ccs ms = do
  addrs <- for ccs $ \_ -> Address <$> newMutVar undefined
  let lo' = extend lo $ Addr <$> addrs
  (lo<$) . sequence_ $ zipWith (flip poke . buildClosure gl lo') ccs addrs

instance Default (MachineState m) where
  def = MachineState [] [] empty

makeLenses ''Closure
makeLenses ''MachineState

pushArgs :: [Value m] -> MachineState m -> MachineState m
pushArgs as = over args (as ++)

pushUpdate :: Address m -> MachineState m -> MachineState m
pushUpdate addr ms = push (Update addr $ ms^.args) ms & args .~ []

push :: Frame m -> MachineState m -> MachineState m
push fr = stack %~ (fr:)

pop :: MachineState m -> Maybe (Frame m, MachineState m)
pop ms = case ms^.stack of f:fs -> Just (f, ms & stack .~ fs) ; [] -> Nothing

select :: Continuation -> Tag -> G
select (Cont bs df) t =
  fromMaybe (error "PANIC: missing default case in branch") $
    snd <$> lookup t bs <|> df

eval :: (Applicative m, PrimMonad m)
     => MachineState m -> G -> LEnv m -> m (Either (Closure m) Word64)
eval ms (App f xs0) lo = case f of
  Ref r -> case resolve gl lo r of
    Addr a -> enter (pushArgs xs ms) a
    Prim w | null xs0  -> returnLit ms w
           | otherwise -> error "PANIC: primitive applied to arguments"
  Con t -> returnCon ms t xs
 where
 gl = ms^.genv
 xs = resolve gl lo <$> xs0
eval ms (Let bs e) lo = do
  cs <- traverse (allocClosure (ms^.genv) lo) bs
  let w = fresh lo
  eval ms e (fromList (zip [w ..] . fmap Addr $ cs) `union` lo)
eval ms (LetRec bs e) lo = do
  lo' <- allocRecursive (ms^.genv) lo bs
  eval ms e lo'
eval ms (Case co k) lo = eval (push (Branch k lo) ms) co lo
eval ms (Lit l) _ = returnLit ms l

enter :: (Applicative m, PrimMonad m)
      => MachineState m -> Address m -> m (Either (Closure m) Word64)
enter ms addr = peek addr >>= \case
  cl@(Closure co da)
    | co^.update -> eval (pushUpdate addr ms) (co^.body) da -- (fromList $ zip [0..] da)
    | ar <= length argz -> eval (ms & args .~ rest) (co^.body) lenv
    | otherwise -> case pop ms of
      Just (Update ad st, ms') -> do
        let nf = co^.freeArity + genericLength argz
            nb = co^.boundArity - genericLength argz
        poke ad $ Closure (noUpdate nf nb $ co^.body) (Addr addr : argz)
        enter (ms' & args %~ (++st)) addr
      Just (Branch _ _,_) -> error "under-applied function in branch"
      Nothing -> return $ Left cl
   where
   argz = ms^.args
   ar = fromIntegral $ co^.boundArity
   (first, rest) = splitAt ar argz
   lenv = fromList $ zip [0..] (da ++ first)

returnCon :: (Applicative m, PrimMonad m)
          => MachineState m -> Tag -> [Value m] -> m (Either (Closure m) Word64)
returnCon ms t vs = case pop ms of
  Just (Branch k lo, ms') -> eval ms' (select k t) (extend lo vs)
  Just (Update ad st, ms')
    | null $ ms'^.args -> do
      poke ad (Closure (standardConstructor (genericLength vs) t) vs)
      returnCon (ms' & args .~ st) t vs
    | otherwise -> error "PANIC: update with non-empty arg stack"
  Nothing -> return . Left $
    Closure (standardConstructor (genericLength vs) t) vs

returnLit :: PrimMonad m => MachineState m -> Word64 -> m (Either (Closure m) Word64)
returnLit _ _ = error "returnLit: unimplemented"
