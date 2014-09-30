module Native.Maybe where

import Maybe

toMaybe# : Maybe a -> Maybe# a
toMaybe# Nothing = Nothing#
toMaybe# (Just a) = Just# a

-- builtin
--   data Maybe# (a : *)
--   Just# : a -> Maybe# a
--   Nothing# : Maybe# a
