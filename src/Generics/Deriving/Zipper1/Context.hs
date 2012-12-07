{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

--------------------------------------------------------------------------------

module Generics.Deriving.Zipper1.Context (
  Ctx1(..),
) where

--------------------------------------------------------------------------------

import GHC.Generics

--------------------------------------------------------------------------------

data family Ctx1 (f :: * -> *) p

data    instance Ctx1 U1         p
data    instance Ctx1 (K1 i a)   p
newtype instance Ctx1 (M1 i c f) p = CM (Ctx1 f p)
data    instance Ctx1 Par1       p = CPar
newtype instance Ctx1 (Rec1 f)   p = CRec (Ctx1 (Rep1 f) p)
data    instance Ctx1 (f :+: g)  p = CL (Ctx1 f p)
                                   | CR (Ctx1 g p)
data    instance Ctx1 (f :*: g)  p = C1 (g p) (Ctx1 f p)
                                   | C2 (f p) (Ctx1 g p)

data    instance Ctx1 ([]       :.: g) p = CCons1 [g p] (Ctx1 g p)
                                         | CCons2 (g p) (Ctx1 ([] :.: g) p)
newtype instance Ctx1 (Maybe    :.: g) p = CJust (Ctx1 g p)
newtype instance Ctx1 (Either a :.: g) p = CRight (Ctx1 g p)

-- Remove these once representation types have Generic1 instances. Use GFunctor
-- instead.
instance Functor (Ctx1 U1) where
  fmap = error "impossible"
instance Functor (Ctx1 (K1 i a)) where
  fmap = error "impossible"
instance Functor (Ctx1 f) => Functor (Ctx1 (M1 i c f)) where
  fmap f (CM x) = CM (fmap f x)
instance Functor (Ctx1 Par1) where
  fmap f CPar = CPar
instance Functor (Ctx1 (Rep1 f)) => Functor (Ctx1 (Rec1 f)) where
  fmap f (CRec x) = CRec (fmap f x)
instance (Functor (Ctx1 f), Functor (Ctx1 g)) => Functor (Ctx1 (f :+: g)) where
  fmap f (CL x) = CL (fmap f x)
  fmap f (CR x) = CR (fmap f x)
instance (Functor f, Functor g, Functor (Ctx1 f), Functor (Ctx1 g))
         => Functor (Ctx1 (f :*: g)) where
  fmap f (C1 x c) = C1 (fmap f x) (fmap f c)
  fmap f (C2 x c) = C2 (fmap f x) (fmap f c)

-- Remove these once representation types have Generic1 instances. Use GFunctor
-- instead.
instance Functor U1 where
  fmap _ U1 = U1
instance Functor (K1 i a) where
  fmap _ (K1 x) = K1 x
instance Functor f => Functor (M1 i c f) where
  fmap f (M1 x) = M1 (fmap f x)
instance Functor Par1 where
  fmap f (Par1 p) = Par1 (f p)
instance Functor f => Functor (Rec1 f) where
  fmap f (Rec1 x) = Rec1 (fmap f x)
instance (Functor f, Functor g) => Functor (f :+: g) where
  fmap f (L1 x) = L1 (fmap f x)
  fmap f (R1 x) = R1 (fmap f x)
instance (Functor f, Functor g) => Functor (f :*: g) where
  fmap f (x :*: y) = fmap f x :*: fmap f y
instance (Functor f, Functor g) => Functor (f :.: g) where
  fmap f (Comp1 x) = Comp1 (fmap (fmap f) x)

