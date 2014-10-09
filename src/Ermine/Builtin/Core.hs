{-# LANGUAGE TupleSections #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett and Dan Doel 2013-2014
-- License   :  BSD2
-- Maintainer:  Dan Doel <dan.doel@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
-- $setup
-- >>> :m + Text.Groom Ermine.Builtin.Core Data.Int
--
-- >>> putStrLn $ groom (lit (1 :: Int32) `cons` nil :: Core Convention a)
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
  , cShowLongHash
  , cAddLong
  , cFromIntegerToInt
  , cFromIntegerToLong
  ) where

import Bound
import Control.Lens ((#), review)
import Data.Functor
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
nil :: Core cc a
nil = Data [] 0 nilg []

-- | The built-in '::' constructor for a list.
cons :: AsConvention cc => Core cc a -> Core cc a -> Core cc a
cons a as = Data (review _Convention <$> [C,C]) 1 consg [a,as]

-- | The built-in 'Just' constructor for 'Maybe'.
just :: AsConvention cc => Core cc a -> Core cc a
just a = Data [_Convention # C] 1 justg [a]

-- | The built-in 'Nothing' constructor for 'Maybe'.
nothing :: Core cc a
nothing = Data [] 0 nothingg []

stringh :: AsConvention cc => Core cc a -> Core cc a
stringh s = Data [_Convention # N] 1 stringhg [s]

inth :: AsConvention cc => Core cc a -> Core cc a
inth i = Data [_Convention # U] 1 inthg [i]

longh :: AsConvention cc => Core cc a -> Core cc a
longh l = Data [_Convention # U] 1 longhg [l]

cPutStrLn :: AsConvention cc => Core cc a
cPutStrLn =
  Lam [_Convention # C] . Scope $
    Case (Var (B 0))
      (M.singleton 0 . Match [_Convention # N] stringg
        . Scope . App (_Convention # N) (_Id._Global # putStrLng) $ Var (B 1))
      Nothing

cShowInt :: AsConvention cc => Core cc a
cShowInt =
  Lam [_Convention # C] . Scope $
    Case (Var (B 0))
      (M.singleton 0 . Match [_Convention # U] inthg
        . Scope . App (_Convention # U) (_Id._Global # showIntg) $ Var (B 1))
      Nothing

cShowLong :: AsConvention cc => Core cc a
cShowLong =
  Lam [_Convention # C] . Scope $
    Case (Var (B 0))
      (M.singleton 0 . Match [_Convention # U] longhg
        . Scope . App (_Convention # U) (_Id._Global # showLongg) $ Var (B 1))
      Nothing

cShowLongHash :: AsConvention cc => Core cc a
cShowLongHash = _Id._Global # showLongg

cAddLong :: AsConvention cc => Core cc a
cAddLong =
  Lam (review _Convention <$> [C,C]) . Scope $
    Case (Var (B 0))
      (M.singleton 0 . Match [_Convention # U] longhg . Scope
        $ Case (Var . F . Var . B $ 1)
            (M.singleton 0 . Match [_Convention # U] longhg . Scope
              $ App (_Convention # U)
                  (App (_Convention # U)
                    (_Id._Global # addLongg) $ (Var . F . Var . B $ 1))
                  (Var $ B 1))
            Nothing)
      Nothing

cFromIntegerToInt :: AsConvention cc => Core cc a
cFromIntegerToInt =
  Lam [_Convention # C] . Scope $
    Case (Var (B 0))
      (M.singleton 0 . Match [_Convention # N] integerhg . Scope
        . App (_Convention # N) (_Id._Global # fromIntegerToIntg) $ Var (B 1))
      Nothing

cFromIntegerToLong :: AsConvention cc => Core cc a
cFromIntegerToLong =
  Lam [_Convention # C] . Scope $
    Case (Var (B 0))
      (M.singleton 0 . Match [_Convention # N] integerhg . Scope
        . App (_Convention # N) (_Id._Global # fromIntegerToLongg) $ Var (B 1))
      Nothing

-- | Lifting of literal values to core.
class Lit a where
  lit  :: AsConvention cc => a   -> Core cc b
  lits :: AsConvention cc => [a] -> Core cc b
  lits = Prelude.foldr (cons . lit) nil

instance Lit Int64 where
  lit l = Data [_Convention # U] 0 literalg [HardCore $ Lit $ Long l]

instance Lit Int32 where
  lit i = Data [_Convention # U] 0 literalg [HardCore $ Lit $ Int i]

instance Lit Char where
  lit c  = Data [_Convention # U] 0 literalg [HardCore $ Lit $ Char c]
  lits = lit . pack

instance Lit Text where
  lit s = Data [_Convention # N] 0 stringg [HardCore $ Lit $ String s]

instance Lit Int8 where
  lit b = Data [_Convention # U] 0 literalg [HardCore $ Lit $ Byte b]

instance Lit Int16 where
  lit s = Data [_Convention # U] 0 literalg [HardCore $ Lit $ Short s]

instance (Lit a, Lit b) => Lit (a, b) where
  lit (a,b) = Data (review _Convention <$> [C,C]) 0 (tupleg 2) [lit a, lit b]

instance Lit a => Lit [a] where
  lit = lits

instance Lit a => Lit (Maybe a) where
  lit = maybe nothing (just . lit)
