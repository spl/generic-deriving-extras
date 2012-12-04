{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

--------------------------------------------------------------------------------

module Generics.Deriving.Zipper.Context (
  Ctx(..),
) where

--------------------------------------------------------------------------------

import GHC.Generics

--------------------------------------------------------------------------------

data family Ctx (f :: * -> *) p :: *

data instance Ctx U1 p

data instance Ctx (K1 i a) p = CK

newtype instance Ctx (M1 i c f) p = CM (Ctx f p)

data instance Ctx (f :+: g) p = CL (Ctx f p)
                              | CR (Ctx g p)

data instance Ctx (f :*: g) p = C1 (g p) (Ctx f p)
                              | C2 (f p) (Ctx g p)

