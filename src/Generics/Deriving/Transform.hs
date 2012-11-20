{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}

module Generics.Deriving.Transform (
  TransformUp(..),
  transformUpdefault,
  TransformUpM(..),
  transformUpMdefault,
) where

--------------------------------------------------------------------------------

import GHC.Generics
import Control.Monad

--------------------------------------------------------------------------------

-- | Type-indexed function for transforming bottom-up
class TransformUp' f a where
  transformUp' :: (a -> a) -> f b -> f b

instance TransformUp' U1 a where
  transformUp' _ U1 = U1

instance TransformUp' (K1 i b) b where
  transformUp' f (K1 x) = K1 (f x)

instance TransformUp b a => TransformUp' (K1 i b) a where
  transformUp' f (K1 x) = K1 (transformUp f x)

instance TransformUp' f a => TransformUp' (M1 i c f) a where
  transformUp' f (M1 x) = M1 (transformUp' f x)

instance (TransformUp' f a, TransformUp' h a) => TransformUp' (f :+: h) a where
  transformUp' f (L1 x) = L1 (transformUp' f x)
  transformUp' f (R1 x) = R1 (transformUp' f x)

instance (TransformUp' f a, TransformUp' h a) => TransformUp' (f :*: h) a where
  transformUp' f (x :*: y) = transformUp' f x :*: transformUp' f y

--------------------------------------------------------------------------------

transformUpdefault :: (Generic b, TransformUp' (Rep b) a) => (a -> a) -> b -> b
transformUpdefault f = to . transformUp' f . from

--------------------------------------------------------------------------------

class TransformUp b a where
  transformUp :: (a -> a) -> b -> b

  default transformUp :: (Generic b, TransformUp' (Rep b) a) => (a -> a) -> b -> b
  transformUp = transformUpdefault

instance TransformUp Char   a where transformUp _ = id
instance TransformUp Int    a where transformUp _ = id
instance TransformUp Float  a where transformUp _ = id
instance TransformUp Bool   a where transformUp _ = id

--------------------------------------------------------------------------------

-- | Type-indexed function for transforming with a monadic function bottom-up
class TransformUpM' f a where
  transformUpM' :: Monad m => (a -> m a) -> f b -> m (f b)

instance TransformUpM' U1 a where
  transformUpM' _ U1 = return U1

instance TransformUpM' (K1 i a) a where
  transformUpM' f (K1 x) = liftM K1 (f x)

instance TransformUpM b a => TransformUpM' (K1 i b) a where
  transformUpM' f (K1 x) = liftM K1 (transformUpM f x)

instance TransformUpM' f b => TransformUpM' (M1 i c f) b where
  transformUpM' f (M1 x) = liftM M1 (transformUpM' f x)

instance (TransformUpM' f b, TransformUpM' h b) => TransformUpM' (f :+: h) b where
  transformUpM' f (L1 x) = liftM L1 (transformUpM' f x)
  transformUpM' f (R1 x) = liftM R1 (transformUpM' f x)

instance (TransformUpM' f b, TransformUpM' h b) => TransformUpM' (f :*: h) b where
  transformUpM' f (x :*: y) = liftM2 (:*:) (transformUpM' f x) (transformUpM' f y)

--------------------------------------------------------------------------------

transformUpMdefault :: (Generic b, TransformUpM' (Rep b) a, Monad m) => (a -> m a) -> b -> m b
transformUpMdefault f = liftM to . transformUpM' f . from

--------------------------------------------------------------------------------

class TransformUpM b a where
  transformUpM :: Monad m => (a -> m a) -> b -> m b

  default transformUpM :: (Generic b, TransformUpM' (Rep b) a, Monad m) => (a -> m a) -> b -> m b
  transformUpM = transformUpMdefault

instance TransformUpM Char   a where transformUpM _ = return
instance TransformUpM Int    a where transformUpM _ = return
instance TransformUpM Float  a where transformUpM _ = return
instance TransformUpM Bool   a where transformUpM _ = return

