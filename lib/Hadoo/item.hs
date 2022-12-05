module Item where

data State = Todo | Started | Done deriving (Show, Eq, Bounded, Enum)
