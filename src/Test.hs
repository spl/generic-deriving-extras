{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# OPTIONS -ddump-deriv #-}

module Test where

import GHC.Generics
import Generics.Deriving.Fold1

data T a = C [T a] [Int] [a] | D
  deriving (Generic1, Functor)

data ListF a r = NilF | ConsF [a] r
  deriving Generic1

instance Fold1 (ListF a)

ex = fold1 (0,\xs r -> sum xs + r) (In (ConsF [1,2] (In (ConsF [3,4] (In NilF)))))

