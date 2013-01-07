module Ermine.Kind.Parser
  ( kind
  ) where

import Control.Applicative
import Control.Lens
import Data.HashSet
import Ermine.Kind
import Text.Parser.Combinators
import Text.Parser.Token
import Text.Parser.Token.Style

kid :: TokenParsing m => IdentifierStyle m
kid = haskellIdents
  & styleName  .~ "kind variable"
  & styleReserved .~ fromList ["rho","ρ","phi","φ","constraint","Γ"]

kind0 :: (Monad m, TokenParsing m) => m (Kind String)
kind0 = parens kind
    <|> star <$ symbol "*"
    <|> rho <$ reserve kid "rho"
    <|> rho <$ reserve kid "ρ"
    <|> phi <$ reserve kid "phi"
    <|> phi <$ reserve kid "φ"
    <|> constraint <$ reserve kid "constraint"
    <|> constraint <$ reserve kid "Γ"
    <|> Var <$> ident kid

kind :: (Monad m, TokenParsing m) => m (Kind String)
kind = chainr1 kind0 ((:->) <$ symbol "->")
