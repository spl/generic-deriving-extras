{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

--------------------------------------------------------------------------------

module Generics.Deriving.Fold1 where

--------------------------------------------------------------------------------

import GHC.Generics

-- This is for the Functor instances. Swap with GFunctor later.
import Generics.Deriving.Zipper1.Context
-- import Generics.Deriving.Functor (GFunctor(..))

--------------------------------------------------------------------------------

-- Note: We do not know (according to the types) whether a Rec1 holds a
-- recursive position or a constant. So, we use Either to allow for both
-- options. This makes the algebra ugly, since we need to use Left/Right at
-- every Rec1. There is also no error checking at the moment, but it can be
-- added later.
--
-- One way to know if a Rec1 is recursive or not is to use the upcoming
-- overlapping type families.

type family Alg1 (f :: * -> *) r

type instance Alg1 U1                      r = r
type instance Alg1 (K1 i a)                r = a -> r
type instance Alg1 (M1 i c f)              r = Alg1 f r
type instance Alg1 Par1                    r = r -> r
type instance Alg1 (Rec1 f)                r = Either (f r) r -> r
type instance Alg1 (f :+: g)               r = (Alg1 f r, Alg1 g r)
type instance Alg1 (M1 i c (K1 j a) :*: g) r = a -> Alg1 g r
type instance Alg1 (M1 i c Par1 :*: g)     r = r -> Alg1 g r
type instance Alg1 (M1 i c (Rec1 f) :*: g) r = Either (f r) r -> Alg1 g r
type instance Alg1 ((f :*: g) :*: h)       r = Alg1 (f :*: (g :*: h)) r
-- type instance Alg1 (f :.: Par1)            r =

--------------------------------------------------------------------------------

class Fold1' f t where
  apply1' :: t a -> Alg1 (Rep1 t) r -> Alg1 f r -> f r -> r

--------------------------------------------------------------------------------

instance Fold1' U1 t where
  apply1' _ _ alg U1 = alg

instance Fold1' (K1 i a) t where
  apply1' _ _ alg (K1 x) = alg x

instance Fold1' f t => Fold1' (M1 i c f) t where
  apply1' p talg alg (M1 x) = apply1' p talg alg x

instance Fold1' Par1 t where
  apply1' _ _ alg (Par1 x) = alg x

instance (Generic1 t, Fold1' (Rep1 t) t) => Fold1' (Rec1 t) t where
  apply1' p talg alg (Rec1 x) = alg (Right (apply1' p talg talg (from1 x)))

instance Fold1' (Rec1 f) t where
  apply1' _ _ alg (Rec1 x) = alg (Left x)

instance (Fold1' f t, Fold1' g t) => Fold1' (f :+: g) t where
  apply1' p talg (alg, _) (L1 x) = apply1' p talg alg x
  apply1' p talg (_, alg) (R1 x) = apply1' p talg alg x

instance Fold1' g t => Fold1' (M1 i c (K1 j a) :*: g) t where
  apply1' p talg alg (M1 (K1 x) :*: g) = apply1' p talg (alg x) g

instance Fold1' g t => Fold1' (M1 i c Par1 :*: g) t where
  apply1' p talg alg (M1 (Par1 x) :*: g) = apply1' p talg (alg x) g

instance (Generic1 t, Fold1' (Rep1 t) t, Fold1' g t) => Fold1' (M1 i c (Rec1 t) :*: g) t where
  apply1' p talg alg (M1 (Rec1 x) :*: g) = apply1' p talg (alg (Right (apply1' p talg talg (from1 x)))) g

instance Fold1' g t => Fold1' (M1 i c (Rec1 f) :*: g) t where
  apply1' p talg alg (M1 (Rec1 x) :*: g) = apply1' p talg (alg (Left x)) g

instance Fold1' (f :*: (g :*: h)) t => Fold1' ((f :*: g) :*: h) t where
  apply1' p talg alg ((f :*: g) :*: h) = apply1' p talg alg (f :*: (g :*: h))

--------------------------------------------------------------------------------

newtype Fix f = In { out :: f (Fix f) }

class (Generic1 f, Functor (Rep1 f), Fold1' (Rep1 f) f) => Fold1 f where
  fold1 :: Alg1 (Rep1 f) r -> Fix f -> r
  fold1 alg (In x) = apply1' (undefined :: f a) alg alg . fmap (fold1 alg) . from1 $ x

--------------------------------------------------------------------------------

instance Fold1 []
instance Fold1 Maybe
instance Fold1 (Either a)

instance Fold1 ((,) a)
instance Fold1 ((,,) a b)
instance Fold1 ((,,,) a b c)
instance Fold1 ((,,,,) a b c d)
instance Fold1 ((,,,,,) a b c d e)
instance Fold1 ((,,,,,,) a b c d e f)

