module Void where

data Void
data Bound a
data Unbound a

absurd : Void -> a
absurd x = case x of {}
