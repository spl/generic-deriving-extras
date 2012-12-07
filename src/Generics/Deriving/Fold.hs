{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

--------------------------------------------------------------------------------

module Generics.Deriving.Fold where

--------------------------------------------------------------------------------

import GHC.Generics

--------------------------------------------------------------------------------

-- Note: We do not know (according to the types) whether a K1 holds a recursive
-- position or a constant. So, we use Either to allow for both options. This
-- makes the algebra ugly, since we need to use Left/Right at every K1. There is
-- also no error checking at the moment, but it can be added later.
--
-- One way to know if a K1 is recursive or not is to use the upcoming
-- overlapping type families.

type family Alg (f :: * -> *) r

type instance Alg U1                      r = r
type instance Alg (K1 i a)                r = Either a r -> r
type instance Alg (M1 i c f)              r = Alg f r
type instance Alg (f :+: g)               r = (Alg f r, Alg g r)
type instance Alg (M1 i c (K1 j a) :*: g) r = Either a r -> Alg g r
type instance Alg ((f :*: g) :*: h)       r = Alg (f :*: (g :*: h)) r

--------------------------------------------------------------------------------

class Fold' f a where
  fold' :: proxy a -> Alg (Rep a) r -> Alg f r -> f x -> r

--------------------------------------------------------------------------------

instance Fold' U1 a where
  fold' _ _ alg U1 = alg

instance Fold a => Fold' (K1 i a) a where
  fold' p palg alg (K1 x) = alg (Right (fold palg x))

instance Fold' (K1 i b) a where
  fold' p _ alg (K1 x) = alg (Left x)

instance Fold' f a => Fold' (M1 i c f) a where
  fold' p palg alg (M1 x) = fold' p palg alg x

instance (Fold' f a, Fold' g a) => Fold' (f :+: g) a where
  fold' p palg (alg, _) (L1 x) = fold' p palg alg x
  fold' p palg (_, alg) (R1 x) = fold' p palg alg x

instance (Fold a, Fold' g a) => Fold' (M1 i c (K1 j a) :*: g) a where
  fold' p palg alg (M1 (K1 x) :*: g) = fold' p palg (alg (Right (fold palg x))) g

instance Fold' g a => Fold' (M1 i c (K1 j b) :*: g) a where
  fold' p palg alg (M1 (K1 x) :*: g) = fold' p palg (alg (Left x)) g

instance Fold' (f :*: (g :*: h)) a => Fold' ((f :*: g) :*: h) a where
  fold' p palg alg ((f :*: g) :*: h) = fold' p palg alg (f :*: (g :*: h))

--------------------------------------------------------------------------------

class (Generic a, Fold' (Rep a) a) => Fold a where
  fold :: Alg (Rep a) r -> a -> r
  fold alg x = fold' (Just x) alg alg (from x)

--------------------------------------------------------------------------------

instance Fold Bool
instance Fold Char
instance Fold Double
instance Fold Float
instance Fold Int
instance Fold Ordering

instance Fold [a]
instance Fold (Maybe a)
instance Fold (Either a b)

instance Fold ()
instance Fold (a, b)
instance Fold (a, b, c)
instance Fold (a, b, c, d)
instance Fold (a, b, c, d, e)
instance Fold (a, b, c, d, e, f)
instance Fold (a, b, c, d, e, f, g)

