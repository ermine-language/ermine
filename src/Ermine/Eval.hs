{-# LANGUAGE GADTs #-}

--------------------------------------------------------------------
-- |
-- Module    :  Ermine.Eval
-- Copyright :  (c) Dan Doel 2013
-- License   :  BSD3
-- Maintainer:  Dan Doel <dan.doel@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--------------------------------------------------------------------
module Ermine.Eval where

import Ermine.Syntax.Core

data Runtime = PAp Runtime [Runtime]
             | Fun !Int ([Runtime] -> Runtime)

data Cxt = Apply [Runtime]

data Env a where

ref :: Env a -> a -> Runtime
ref = undefined

eval :: Env a -> [Cxt] -> Core a -> Runtime
eval env cxt (Var v)        = ref env v
eval env cxt (HardCore h)   = undefined
eval env cxt (App f x)      = unapply env [eval env [] x] f
eval env cxt (Lam n b)      = undefined
eval env cxt (Let ds b)     = undefined
eval env cxt (Case e bs)    = undefined
eval env cxt (Dict sus sls) = undefined
eval env cxt (LamDict b)    = undefined
eval env cxt (AppDict f x)  = undefined

unapply env cxt stk (App f x) = unapply env cxt (eval env [] x : stk) f
unapply env cxt stk e         = eval env (Apply stk : cxt) e


