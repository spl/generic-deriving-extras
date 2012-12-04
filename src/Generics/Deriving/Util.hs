
--------------------------------------------------------------------------------

module Generics.Deriving.Util (
  Dir(..),
  dir,
) where

--------------------------------------------------------------------------------

data Dir = L | R deriving (Show, Eq)

dir :: Dir -> a -> a -> a
dir L f _ = f
dir R _ g = g

