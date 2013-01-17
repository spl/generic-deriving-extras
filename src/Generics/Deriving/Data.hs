{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}

--------------------------------------------------------------------------------

module Generics.Deriving.Data where

--------------------------------------------------------------------------------

import GHC.Generics
import Data.Data (Data)

--------------------------------------------------------------------------------

class Data' f where
  gfoldl' :: (forall a b. Generic a => c (a -> b) -> a -> c b)
          -> (forall a. a -> c a)
          -> f x
          -> c (f x)

--------------------------------------------------------------------------------

instance Data' (C1 c U1) where
  gfoldl' _ z c = z c

instance Data' (C1 c (S1 c (Rec0 a))) where
  gfoldl' _ z (M1 (M1 x)) = z (M1 U1)

{-

instance (Data' f a, Data' g a) => Data' (f :+: g) a where
  gfoldl' p palg (alg, _) (L1 x) = gfoldl' p palg alg x
  gfoldl' p palg (_, alg) (R1 x) = gfoldl' p palg alg x

instance (Data a, Data' g a) => Data' (M1 i c (K1 j a) :*: g) a where
  gfoldl' p palg alg (M1 (K1 x) :*: g) = gfoldl' p palg (alg (Right (gfoldl palg x))) g

instance Data' g a => Data' (M1 i c (K1 j b) :*: g) a where
  gfoldl' p palg alg (M1 (K1 x) :*: g) = gfoldl' p palg (alg (Left x)) g

instance Data' (f :*: (g :*: h)) a => Data' ((f :*: g) :*: h) a where
  gfoldl' p palg alg ((f :*: g) :*: h) = gfoldl' p palg alg (f :*: (g :*: h))

--------------------------------------------------------------------------------

class (Generic a, Data' (Rep a) a) => Data a where
  gfoldl :: Alg (Rep a) r -> a -> r
  gfoldl alg x = gfoldl' (Just x) alg alg (from x)

--------------------------------------------------------------------------------

instance Data Bool
instance Data Char
instance Data Double
instance Data Float
instance Data Int
instance Data Ordering

instance Data [a]
instance Data (Maybe a)
instance Data (Either a b)

instance Data ()
instance Data (a, b)
instance Data (a, b, c)
instance Data (a, b, c, d)
instance Data (a, b, c, d, e)
instance Data (a, b, c, d, e, f)
instance Data (a, b, c, d, e, f, g)
-}

