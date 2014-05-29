{-# LANGUAGE TupleSections #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett and Dan Doel 2013-2014
-- License   :  BSD3
-- Maintainer:  Dan Doel <dan.doel@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
-- $setup
-- >>> :m + Text.Groom Ermine.Builtin.Core Data.Int
--
-- >>> putStrLn $ groom $ lit (1 :: Int32) `cons` nil
-- Data [C, C] 1
--   (glob (Infix R 5) (mkModuleName "ermine" "Prelude") "(::)")
--   [Data [U] 0
--      (glob Idfix (mkModuleName "ermine" "Prelude") "Literal")
--      [HardCore (Lit (Int 1))],
--    Data [] 0 (glob Idfix (mkModuleName "ermine" "Prelude") "[]") []]
--------------------------------------------------------------------
module Ermine.Builtin.Core
  (
  -- * Host Literals
    Lit(..)
  -- * Lists
  , cons
  , nil
  -- * Maybe
  , just
  , nothing
  -- * prim wrappers
  , stringh
  , inth
  , longh
  -- * primops
  , cPutStrLn
  , cShowInt
  , cShowLong
  , cAddLong
  ) where

import Bound
import Control.Lens ((#))
import Data.Int
import qualified Data.Map as M
import Data.Text hiding (zip, length, concatMap, cons)
import Ermine.Builtin.Global
import Ermine.Syntax.Convention
import Ermine.Syntax.Core
import Ermine.Syntax.Global hiding (N)
import Ermine.Syntax.Id
import Ermine.Syntax.Literal

-- | The built-in '[]' constructor for a list.
nil :: Core a
nil = Data [] 0 nilg []

-- | The built-in '::' constructor for a list.
cons :: Core a -> Core a -> Core a
cons a as = Data [C,C] 1 consg [a,as]

-- | The built-in 'Just' constructor for 'Maybe'.
just :: Core a -> Core a
just a = Data [C] 1 justg [a]

-- | The built-in 'Nothing' constructor for 'Maybe'.
nothing :: Core a
nothing = Data [] 0 nothingg []

stringh :: Core a -> Core a
stringh s = Data [N] 1 stringhg [s]

inth :: Core a -> Core a
inth i = Data [U] 1 inthg [i]

longh :: Core a -> Core a
longh l = Data [U] 1 longhg [l]

cPutStrLn :: Core a
cPutStrLn =
  Lam [C] . Scope $
    Case (Var (B 0))
      (M.singleton 0 . Match [N] stringg
        . Scope . App N (_Id._Global # putStrLng) $ Var (B 1))
      Nothing

cShowInt :: Core a
cShowInt =
  Lam [C] . Scope $
    Case (Var (B 0))
      (M.singleton 0 . Match [U] inthg
        . Scope . App U (_Id._Global # showIntg) $ Var (B 1))
      Nothing

cShowLong :: Core a
cShowLong =
  Lam [C] . Scope $
    Case (Var (B 0))
      (M.singleton 0 . Match [U] longhg
        . Scope . App U (_Id._Global # showLongg) $ Var (B 1))
      Nothing

cAddLong :: Core a
cAddLong =
  Lam [C,C] . Scope $
    Case (Var (B 0))
      (M.singleton 0 . Match [U] longhg . Scope
        $ Case (Var . F . Var . B $ 1)
            (M.singleton 0 . Match [U] longhg . Scope
              $ App U
                  (App U (_Id._Global # addLongg) $ (Var . F . Var . B $ 1))
                  (Var $ B 1))
            Nothing)
      Nothing

-- | Lifting of literal values to core.
class Lit a where
  lit  :: a   -> Core b
  lits :: [a] -> Core b
  lits = Prelude.foldr (cons . lit) nil

instance Lit Int64 where
  lit l = Data [U] 0 literalg [HardCore $ Lit $ Long l]

instance Lit Int32 where
  lit i = Data [U] 0 literalg [HardCore $ Lit $ Int i]

instance Lit Char where
  lit c  = Data [U] 0 literalg [HardCore $ Lit $ Char c]
  lits = lit . pack

instance Lit Text where
  lit s = Data [N] 0 stringg [HardCore $ Lit $ String s]

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
