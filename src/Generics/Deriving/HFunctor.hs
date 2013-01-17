{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

--------------------------------------------------------------------------------

module Generics.Deriving.HFunctor where

--------------------------------------------------------------------------------

import GHC.Generics

--------------------------------------------------------------------------------

type Fun p = forall a b. HFunctor p a b => p a b -> a -> b

--------------------------------------------------------------------------------

class HFunctor' f g p where
  hmap' :: p a b -> Fun p -> f x -> g x

--------------------------------------------------------------------------------

instance HFunctor' U1 U1 p where
  hmap' _ _ U1 = U1

instance HFunctor p a b => HFunctor' (K1 i a) (K1 j b) p where
  hmap' _ f (K1 x) = K1 (f proxy x)

instance HFunctor' f g p  => HFunctor' (M1 i c f) (M1 j d g) p where
  hmap' p f (M1 x) = M1 (hmap' p f x)

instance (HFunctor' f1 f2 p, HFunctor' g1 g2 p) => HFunctor' (f1 :*: g1) (f2 :*: g2) p where
  hmap' p f (x :*: y) = hmap' p f x :*: hmap' p f y

instance (HFunctor' f1 f2 p, HFunctor' g1 g2 p) => HFunctor' (f1 :+: g1) (f2 :+: g2) p where
  hmap' p f (L1 x) = L1 (hmap' p f x)
  hmap' p f (R1 x) = R1 (hmap' p f x)

--------------------------------------------------------------------------------

class HFunctor p a b where
  hmap :: p a b -> a -> b
  
  default hmap :: (Generic a, Generic b, HFunctor' (Rep a) (Rep b) p) => p a b -> a -> b
  hmap p x = to (hmap' p hmap (from x))

  proxy :: p a b

