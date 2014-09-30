module Nullable where

import Maybe

-- data Nullable a = Null (Prim a) | Some a

toMaybe : Nullable a -> Maybe a
toMaybe (Null _) = Nothing
toMaybe (Some a) = Just a

fromMaybe : Prim a -> Maybe a -> Nullable a
fromMaybe p Nothing  = Null p
fromMaybe _ (Just a) = Some a

getOrElse : a -> Nullable a -> a
getOrElse z d = maybe z (x -> x) (toMaybe d)

map : Prim b -> (a -> b) -> Nullable a -> Nullable b
map p f (Null _) = Null p
map _ f (Some a) = Some (f a)
