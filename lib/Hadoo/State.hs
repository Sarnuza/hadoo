module Hadoo.State where

data State = Todo | Started | Done deriving (Show, Read, Eq, Bounded, Enum)