{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE RankNTypes #-}

--------------------------------------------------------------------------------

module Generics.Deriving.Transform (
  T,
  TransformUp(..),
  TW,
  TransformUpWith(..),
  transformUpA,
  transformUpM,
  transformUpdefault,
  transformUpapply,
  transformUpempty,
  transformUpWithdefault,
  transformUpWithapply,
  transformUpWithempty,
) where

--------------------------------------------------------------------------------

import GHC.Generics
import Control.Monad (liftM, ap)
import Control.Applicative (Applicative(pure, (<*>)))

--------------------------------------------------------------------------------

type T b a = (a -> a) -> b -> b

-- | The type-indexed function

class TransformUp' f a where
  transformUp' :: T (f x) a

instance TransformUp' U1 a where
  transformUp' _ U1 = U1

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

transformUpdefault :: (Generic b, TransformUp' (Rep b) a) => T b a
transformUpdefault f = to . transformUp' f . from

transformUpapply :: T a a
transformUpapply f = f

transformUpempty :: T b a
transformUpempty _ x = x

--------------------------------------------------------------------------------

class TransformUp b a where
  transformUp :: T b a

  default transformUp :: (Generic b, TransformUp' (Rep b) a) => T b a
  transformUp = transformUpdefault

--------------------------------------------------------------------------------

-- Each type requires two instances.

-- 1. Matching - query type equals result type

-- 2. Default - query type does not equal result type

-- These types (mostly primitive) are the leaves of datatypes, so their default
-- instances need to stop the recursion. Since no other type is found in them
-- type, we use 'collectempty' for the default instance.

instance TransformUp Char Char where transformUp = transformUpapply
instance TransformUp Char b    where transformUp = transformUpempty

instance TransformUp Int Int where transformUp = transformUpapply
instance TransformUp Int b   where transformUp = transformUpempty

instance TransformUp Float Float where transformUp = transformUpapply
instance TransformUp Float b     where transformUp = transformUpempty

instance TransformUp Double Double where transformUp = transformUpapply
instance TransformUp Double b      where transformUp = transformUpempty

instance TransformUp Bool Bool where transformUp = transformUpapply
instance TransformUp Bool b    where transformUp = transformUpempty

instance TransformUp () () where transformUp = transformUpapply
instance TransformUp () b  where transformUp = transformUpempty

-- For all other types, the default instance is standard.

instance TransformUp (Maybe b) (Maybe b) where transformUp = transformUpapply
instance TransformUp b a => TransformUp (Maybe b) a

instance TransformUp [b] [b] where transformUp = transformUpapply
instance TransformUp b a => TransformUp [b] a

instance TransformUp (b,c) (b,c) where transformUp = transformUpapply
instance (TransformUp b a, TransformUp c a) => TransformUp (b,c) a

instance TransformUp (b,c,d) (b,c,d) where transformUp = transformUpapply
instance (TransformUp b a, TransformUp c a, TransformUp d a) => TransformUp (b,c,d) a

instance TransformUp (b,c,d,e) (b,c,d,e) where transformUp = transformUpapply
instance (TransformUp b a, TransformUp c a, TransformUp d a, TransformUp e a) => TransformUp (b,c,d,e) a

instance TransformUp (b,c,d,e,f) (b,c,d,e,f) where transformUp = transformUpapply
instance (TransformUp b a, TransformUp c a, TransformUp d a, TransformUp e a, TransformUp f a) => TransformUp (b,c,d,e,f) a

instance TransformUp (b,c,d,e,f,g) (b,c,d,e,f,g) where transformUp = transformUpapply
instance (TransformUp b a, TransformUp c a, TransformUp d a, TransformUp e a, TransformUp f a, TransformUp g a) => TransformUp (b,c,d,e,f,g) a

instance TransformUp (b,c,d,e,f,g,h) (b,c,d,e,f,g,h) where transformUp = transformUpapply
instance (TransformUp b a, TransformUp c a, TransformUp d a, TransformUp e a, TransformUp f a, TransformUp g a, TransformUp h a) => TransformUp (b,c,d,e,f,g,h) a

--------------------------------------------------------------------------------

type TW b m a = (forall a . a -> m a) -> (forall a b . (a -> b) -> m a -> m b) -> (forall a b . m (a -> b) -> m a -> m b) -> (a -> m a) -> b -> m b

-- | The type-indexed function

class TransformUpWith' f a where
  transformUpWith' :: TW (f x) m a

instance TransformUpWith' U1 a where
  transformUpWith' r _ _ _ U1 = r U1

instance TransformUpWith b a => TransformUpWith' (K1 i b) a where
  transformUpWith' r m a f (K1 x) = m K1 (transformUpWith r m a f x)

instance TransformUpWith' f b => TransformUpWith' (M1 i c f) b where
  transformUpWith' r m a f (M1 x) = m M1 (transformUpWith' r m a f x)

instance (TransformUpWith' f b, TransformUpWith' h b) => TransformUpWith' (f :+: h) b where
  transformUpWith' r m a f (L1 x) = m L1 (transformUpWith' r m a f x)
  transformUpWith' r m a f (R1 x) = m R1 (transformUpWith' r m a f x)

instance (TransformUpWith' f b, TransformUpWith' h b) => TransformUpWith' (f :*: h) b where
  transformUpWith' r m a f (x :*: y) = m (:*:) (transformUpWith' r m a f x) `a` transformUpWith' r m a f y

--------------------------------------------------------------------------------

transformUpWithdefault :: (Generic b, TransformUpWith' (Rep b) a) => TW b m a
transformUpWithdefault r m a f = m to . transformUpWith' r m a f . from

transformUpWithapply :: TW a m a
transformUpWithapply _ _ _ f = f

transformUpWithempty :: TW b m a
transformUpWithempty r _ _ _ = r

--------------------------------------------------------------------------------

class TransformUpWith b a where
  transformUpWith :: TW b m a

  default transformUpWith :: (Generic b, TransformUpWith' (Rep b) a) => TW b m a
  transformUpWith = transformUpWithdefault

--------------------------------------------------------------------------------

-- Each type requires two instances.

-- 1. Matching - query type equals result type

-- 2. Default - query type does not equal result type

-- These types (mostly primitive) are the leaves of datatypes, so their default
-- instances need to stop the recursion. Since no other type is found in them
-- type, we use 'collectempty' for the default instance.

instance TransformUpWith Char Char where transformUpWith = transformUpWithapply
instance TransformUpWith Char b    where transformUpWith = transformUpWithempty

instance TransformUpWith Int Int where transformUpWith = transformUpWithapply
instance TransformUpWith Int b   where transformUpWith = transformUpWithempty

instance TransformUpWith Float Float where transformUpWith = transformUpWithapply
instance TransformUpWith Float b     where transformUpWith = transformUpWithempty

instance TransformUpWith Double Double where transformUpWith = transformUpWithapply
instance TransformUpWith Double b      where transformUpWith = transformUpWithempty

instance TransformUpWith Bool Bool where transformUpWith = transformUpWithapply
instance TransformUpWith Bool b    where transformUpWith = transformUpWithempty

instance TransformUpWith () () where transformUpWith = transformUpWithapply
instance TransformUpWith () b  where transformUpWith = transformUpWithempty

-- For all other types, the default instance is standard.

instance TransformUpWith (Maybe b) (Maybe b) where transformUpWith = transformUpWithapply
instance TransformUpWith b a => TransformUpWith (Maybe b) a

instance TransformUpWith [b] [b] where transformUpWith = transformUpWithapply
instance TransformUpWith b a => TransformUpWith [b] a

instance TransformUpWith (b,c) (b,c) where transformUpWith = transformUpWithapply
instance (TransformUpWith b a, TransformUpWith c a) => TransformUpWith (b,c) a

instance TransformUpWith (b,c,d) (b,c,d) where transformUpWith = transformUpWithapply
instance (TransformUpWith b a, TransformUpWith c a, TransformUpWith d a) => TransformUpWith (b,c,d) a

instance TransformUpWith (b,c,d,e) (b,c,d,e) where transformUpWith = transformUpWithapply
instance (TransformUpWith b a, TransformUpWith c a, TransformUpWith d a, TransformUpWith e a) => TransformUpWith (b,c,d,e) a

instance TransformUpWith (b,c,d,e,f) (b,c,d,e,f) where transformUpWith = transformUpWithapply
instance (TransformUpWith b a, TransformUpWith c a, TransformUpWith d a, TransformUpWith e a, TransformUpWith f a) => TransformUpWith (b,c,d,e,f) a

instance TransformUpWith (b,c,d,e,f,g) (b,c,d,e,f,g) where transformUpWith = transformUpWithapply
instance (TransformUpWith b a, TransformUpWith c a, TransformUpWith d a, TransformUpWith e a, TransformUpWith f a, TransformUpWith g a) => TransformUpWith (b,c,d,e,f,g) a

instance TransformUpWith (b,c,d,e,f,g,h) (b,c,d,e,f,g,h) where transformUpWith = transformUpWithapply
instance (TransformUpWith b a, TransformUpWith c a, TransformUpWith d a, TransformUpWith e a, TransformUpWith f a, TransformUpWith g a, TransformUpWith h a) => TransformUpWith (b,c,d,e,f,g,h) a

--------------------------------------------------------------------------------

transformUpA :: (TransformUpWith b a, Applicative m) => (a -> m a) -> b -> m b
transformUpA = transformUpWith pure fmap (<*>)

transformUpM :: (TransformUpWith b a, Monad m) => (a -> m a) -> b -> m b
transformUpM = transformUpWith return liftM ap

