{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DefaultSignatures #-}

--------------------------------------------------------------------------------

module Generics.Deriving.Fold2 where

--------------------------------------------------------------------------------

import GHC.Generics

--------------------------------------------------------------------------------

type family Alg (f :: * -> *) (r :: * -> *) b

type instance Alg U1                      r b = r b
type instance Alg (K1 i a)                r b = Either a (r a) -> r b
type instance Alg (M1 i c f)              r b = Alg f r b
type instance Alg ((f :+: g) :+: h)       r b = Alg (f :+: (g :+: h)) r b
type instance Alg (M1 i c f :+: g)        r b = (Alg (M1 i c f) r b, Alg g r b)
type instance Alg (M1 i c (K1 j a) :*: g) r b = Either a (r a) -> Alg g r b
type instance Alg ((f :*: g) :*: h)       r b = Alg (f :*: (g :*: h)) r b

--------------------------------------------------------------------------------

type Algebra p r = forall a. p a -> Alg (Rep a) r a

--------------------------------------------------------------------------------

class Fold' f p where
  fold' :: Algebra p r -> Alg f r a -> f x -> r a

--------------------------------------------------------------------------------

instance Fold' U1 p where
  fold' _ alg U1 = alg

instance (Fold a, El p a) => Fold' (K1 i a) p where
  fold' palg alg (K1 x) = alg (foldK1 palg x)

instance Fold' f p => Fold' (M1 i c f) p where
  fold' palg alg (M1 x) = fold' palg alg x

instance (Fold' (f :+: (g :+: h)) p) => Fold' ((f :+: g) :+: h) p where
  fold' palg alg = rotate (fold' palg alg)

instance (Fold' (M1 i c f) p, Fold' g p) => Fold' (M1 i c f :+: g) p where
  fold' palg (alg, _) (L1 x) = fold' palg alg x
  fold' palg (_, alg) (R1 x) = fold' palg alg x

instance (Fold a, El p a, Fold' g p) => Fold' (M1 i c (K1 j a) :*: g) p where
  fold' palg alg (M1 (K1 x) :*: g) = fold' palg (alg (foldK1 palg x)) g

instance Fold' (f :*: (g :*: h)) p => Fold' ((f :*: g) :*: h) p where
  fold' palg alg ((f :*: g) :*: h) = fold' palg alg (f :*: (g :*: h))

--------------------------------------------------------------------------------

rotate :: ((f :+: (g :+: h)) p -> r) -> ((f :+: g) :+: h) p -> r
rotate f (L1 (L1 x)) = f (L1 x)
rotate f (L1 (R1 x)) = f (R1 (L1 x))
rotate f (R1 x)      = f (R1 (R1 x))

foldK1 :: (Fold a, El p a) => Algebra p r -> a -> Either a (r a)
foldK1 palg x = case proxy of
    Nothing -> Left x
    Just p  -> Right (fold palg p x)

--------------------------------------------------------------------------------

class Fold a where
  fold :: Algebra p r -> p a -> a -> r a

  default fold :: (Generic a, Fold' (Rep a) p) => Algebra p r -> p a -> a -> r a
  fold palg p x = fold' palg (palg p) (from x)
--------------------------------------------------------------------------------

class El p a where
  proxy :: Maybe (p a)
  proxy = Nothing

