{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
module Ermine.Parser where

import Control.Applicative
import Control.Lens
import Control.Monad
import Data.Foldable
import Data.Monoid
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Traversable
import Data.Void
import Text.Parser.Char hiding (text)
import Text.Parser.Combinators
import Text.Parser.LookAhead
import Text.Parser.Token
import Text.PrettyPrint.ANSI.Leijen as Pretty hiding (line, (<>), (<$>), empty, braces)
import Text.Trifecta.Combinators
import Text.Trifecta.Delta

data LayoutContext
  = IndentedLayout !Int
  | BracedLayout
  { braceLeft        :: String
  , braceEndsWith    :: Parser ()
  , braceUnmatchedBy :: Parser Void
  , braceRight       :: String
  }

data Location = Location Delta String deriving Show

class HasLocation t where
  location :: Lens' t Location

instance HasLocation Location where
  location = id

instance Eq Location where
  Location a _ == Location b _ = a == b

instance Ord Location where
  compare (Location a _) (Location b _) = compare a b

data ParseState = ParseState
  { _parserLayout   :: [LayoutContext]
  , _parserBol      :: Bool
  , _parserInput    :: String
  , _parserLocation :: {-# UNPACK #-} !Location
  }

data Failure = Failure
  { _message  :: [Doc]
  , _expected :: Set String
  } deriving Show

instance Monoid Failure where
  mempty = Failure [] mempty
  Failure as xs  `mappend` Failure bs ys = Failure (as <|> bs) (mappend xs ys)

data ParseResult a
  = Pure a {-# UNPACK #-} !Failure
  | Fail {-# UNPACK #-} !Failure
  | Commit a !ParseState (Set String)
  | Err !Location {-# UNPACK #-} !Failure
  deriving (Functor, Foldable, Traversable)

newtype Parser a = Parser { runParser :: ParseState -> ParseResult a }
  deriving Functor

makeClassy ''Failure
makeLenses ''ParseState

layoutDepth :: Parser Int
layoutDepth = Parser $ \s -> case s^.parserLayout of 
  IndentedLayout n:_ -> Pure n mempty
  _                  -> Pure 0 mempty

layoutEndsWith :: Parser ()
layoutEndsWith = Parser $ \s -> runParser (views parserLayout go s) s where
  go (BracedLayout _ p _ _:_) = p
  go (IndentedLayout _ : xs)  = go xs
  go []                       = eof
  
instance HasLocation ParseState where
  location = parserLocation

bump :: Char -> String -> Location -> Location
bump '\n' xs (Location d _ ) = Location (d <> delta '\n') xs
bump c    _  (Location d ys) = Location (d <> delta c)    ys 

err :: ParseState -> Failure -> ParseResult a
err s = Err (s^.location)

instance Applicative Parser where
  pure a = Parser $ \_ -> Pure a mempty
  (<*>) = ap

instance Monad Parser where
  return a = Parser $ \_ -> Pure a mempty
  Parser m >>= f = Parser $ \s -> case m s of
    Fail e        -> Fail e
    Err x y       -> Err x y
    Pure a e      -> case runParser (f a) s of
      Pure b e'     -> Pure b (e <> e')
      Fail e'       -> Fail (e <> e')
      Commit b t xs -> Commit b t xs
      Err x y       -> Err x y
    Commit a t xs -> case runParser (f a) t of
      Pure b e      -> Commit b t (xs <> e^.expected)
      Fail e        -> err t (over expected (xs<>) e)
      Commit b u ys -> Commit b u ys
      Err x y       -> Err x y 
  fail e = Parser $ \s -> Fail (Failure [text e] mempty)

-- | race two parsers merging errors, and expected values, while taking the longer successful parse.
race :: Parser a -> Parser a -> Parser a 
race (Parser m) (Parser n) = Parser $ \s -> case m s of
  Pure a e -> case n s of -- old state
    Pure a' e' -> Pure a (e <> e')
    Fail e'    -> Pure a (e <> e')
    r -> r
  Fail e -> case n s of -- old state
    Pure a' e' -> Pure a' e'
    Fail e'    -> Fail (e <> e')
    r -> r
  l@(Commit a t xs) -> case n s of -- old state
    r@(Commit a' t' xs') -> case _parserLocation t `compare` _parserLocation t' of
      LT -> r
      EQ -> Commit a t (xs <> xs')
      GT -> l
    Err el e
      | el == _parserLocation t -> Commit a t (xs <> e^.expected)
      -- fall through
    _ -> l
  l@(Err el e) -> case n s of -- old state
    r@(Err el' e') -> case compare el el' of
      LT -> r
      EQ -> Err el (mappend e e')
      GT -> l
    r@(Commit a t xs)
      | el == _parserLocation t -> Commit a t (e^.expected <> xs)
      -- fall through
    r -> r
     
instance Alternative Parser where
  empty = Parser $ \_ -> Fail (Failure [] mempty)
  Parser m <|> Parser n = Parser $ \s -> case m s of
    Fail e -> case n s of
      Pure a e' -> Pure a (e <> e')
      Fail e'   -> Fail (e <> e')
      r -> r
    r -> r

instance MonadPlus Parser where
  mzero = empty
  mplus = (<|>)

instance Parsing Parser where
  try (Parser m) = Parser $ \s -> case m s of
    Err{} -> Fail (Failure [] mempty)
    r -> r
  Parser m <?> n = Parser $ \s -> case m s of
    Pure a (Failure r@(_:_) _) -> Pure a (Failure r (Set.singleton n))
    Fail   (Failure r       _) -> Fail   (Failure r (Set.singleton n))
    l -> l
  eof = Parser $ \s -> case s^.parserInput of
    "" -> Pure () mempty
    _  -> Fail (Failure [] (Set.singleton "EOF"))
  notFollowedBy (Parser m) = Parser $ \s -> case m s of
    Pure a _     -> Fail  (Failure [text "unexpected" <+> text (show a)] mempty)
    Commit a t _ -> err t (Failure [text "unexpected" <+> text (show a)] mempty)
    _            -> Pure () mempty
  unexpected s = Parser $ \_ -> Fail (Failure [text "unexpected" <+> text s] mempty)

-- carefully avoid flagging this as a mutation if we don't change state so we can backtrack.
setBol :: Bool -> Parser ()
setBol b = Parser $ \s -> if s^.parserBol == b
  then Pure () mempty
  else Commit () (s & parserBol .~ b) mempty
  
instance LookAheadParsing Parser where
  lookAhead (Parser m) = Parser $ \s -> case m s of
    Commit a _ _ -> Pure a mempty
    Err{}        -> Fail mempty
    r            -> r

-- satisfaction without changing beginning of line flag
rawSatisfy :: (Char -> Bool) -> Parser Char
rawSatisfy p = Parser $ \s -> case s^.parserInput of
  c:cs 
    | p c -> Commit c (s & parserInput .~ cs & parserLocation %~ bump c cs) mempty
    -- fall through
  _ -> Fail mempty
  
instance CharParsing Parser where
  satisfy p = Parser $ \s -> case s^.parserInput of
    c:cs
      | p c -> Commit c (s & parserInput .~ cs & parserLocation %~ bump c cs & parserBol .~ False) mempty
      -- fall through
    _ -> Fail mempty

data Token = VirtualSemi | VirtualBrace | Whitespace | Other
  deriving (Eq,Ord,Show,Read)

layout :: Parser Token
layout = undefined

virtualLeftBrace :: Parser ()
virtualRightBrace :: Parser ()
virtualLeftBrace = undefined
virtualRightBrace = undefined

laidout :: Parser a -> Parser [a]
laidout p
   = braces (semiSep p)
 <|> between virtualLeftBrace virtualRightBrace (semiSep p)

parseState :: Parser ParseState
parseState = Parser $ \s -> Pure s mempty

-- stillOnside :: Parser ()
-- stillOnside = ps >>= \case 
--  s -> column s > 
  
instance TokenParsing Parser where
  someSpace = do
    Whitespace <- layout
    return ()
  nesting = undefined -- TODO: we need to update parsers for this, as we don't follow this scheme precisely
  semi = undefined -- TODO
  -- token = stillOnside *> p <* optional someSpace

{-
instance DeltaParsing P where
-}

{-
instance Errable P where
  raiseErr e = P $ \_ -> raiseErr e
-}
