
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

module Ermine.Builtin.Term (dataCon) where

import Data.ByteString.Char8
import Data.String
import Ermine.Builtin.Pat
import Ermine.Syntax.Global
import Ermine.Syntax.Pat
import Ermine.Syntax.Term

-- | Construct a builtin term 'DataCon' for a given 'global' in the @\"ermine\"@ package
dataCon :: Fixity -> String -> String -> Term t v
dataCon f m n = HardTerm . DataCon $ global f (pack "ermine") (pack m) (pack n)

