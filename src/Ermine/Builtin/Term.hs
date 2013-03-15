
--------------------------------------------------------------------
-- |
-- Module    :  Ermine.Builtin.Term
-- Copyright :  (c) Edward Kmett, Dan Doel 2012-2013
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
-- Smart builders for convenient building of terms.
--------------------------------------------------------------------

module Ermine.Builtin.Term ( lam
                           , dataCon
                           ) where

import Bound
import Data.ByteString.Char8
import Data.List as List
import Ermine.Builtin.Pat
import Ermine.Syntax
import Ermine.Syntax.Global
import Ermine.Syntax.Pat
import Ermine.Syntax.Term

lam :: Eq v => Binder v [Pat t] -> Term t v -> Term t v
lam (Binder vs ps) = Lam ps . abstract (`List.elemIndex` vs)

-- | Construct a builtin term 'DataCon' for a given 'global' in the @\"ermine\"@ package
dataCon :: Fixity -> String -> String -> Term t v
dataCon f m n = HardTerm . DataCon $ global f (pack "ermine") (pack m) (pack n)
