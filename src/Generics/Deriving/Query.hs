{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}

module Generics.Deriving.Query (
  QueryWith(..),
  queryWithdefault,
  Collect(..),
  collectdefault,
  collectempty,
) where

--------------------------------------------------------------------------------

import GHC.Generics
import Data.Monoid
import Control.Applicative

--------------------------------------------------------------------------------

-- | Type-indexed function for monoidal querying
class QueryWith' f a where
  queryWith' :: Monoid b => (a -> b) -> f c -> b

instance QueryWith' U1 a where
  queryWith' _ U1 = mempty

instance QueryWith' (K1 i a) a where
  queryWith' f (K1 x) = f x

instance QueryWith b a => QueryWith' (K1 i b) a where
  queryWith' f (K1 x) = queryWith f x

instance QueryWith' f a => QueryWith' (M1 i c f) a where
  queryWith' f (M1 x) = queryWith' f x

instance (QueryWith' f a, QueryWith' h a) => QueryWith' (f :+: h) a where
  queryWith' f (L1 x) = queryWith' f x
  queryWith' f (R1 x) = queryWith' f x

instance (QueryWith' f a, QueryWith' h a) => QueryWith' (f :*: h) a where
  queryWith' f (x :*: y) = queryWith' f x `mappend` queryWith' f y

--------------------------------------------------------------------------------

queryWithdefault :: (Generic c, QueryWith' (Rep c) a, Monoid b) => (a -> b) -> c -> b
queryWithdefault f = queryWith' f . from

--------------------------------------------------------------------------------

class QueryWith c a where
  queryWith :: Monoid b => (a -> b) -> c -> b

  default queryWith :: (Generic c, QueryWith' (Rep c) a, Monoid b) => (a -> b) -> c -> b
  queryWith = queryWithdefault

instance QueryWith Char   a where queryWith _ = mempty
instance QueryWith Int    a where queryWith _ = mempty
instance QueryWith Float  a where queryWith _ = mempty
instance QueryWith Bool   a where queryWith _ = mempty

--------------------------------------------------------------------------------

-- | Type-indexed function for alternative collecting
class Collect' f a where
  collect' :: Alternative g => f c -> g a

instance Collect' U1 a where
  collect' U1 = empty

instance Collect b a => Collect' (K1 i b) a where
  collect' (K1 x) = collect x

instance Collect' f a => Collect' (M1 i c f) a where
  collect' (M1 x) = collect' x

instance (Collect' f a, Collect' h a) => Collect' (f :+: h) a where
  collect' (L1 x) = collect' x
  collect' (R1 x) = collect' x

instance (Collect' f a, Collect' h a) => Collect' (f :*: h) a where
  collect' (x :*: y) = collect' x <|> collect' y

--------------------------------------------------------------------------------

collectdefault :: (Generic c, Collect' (Rep c) a, Alternative f) => c -> f a
collectdefault = collect' . from

collectempty :: Alternative f => b -> f a
collectempty _ = empty

--------------------------------------------------------------------------------

class Collect b a where
  collect :: Alternative f => b -> f a

  default collect :: (Generic b, Collect' (Rep b) a, Alternative f) => b -> f a
  collect = collectdefault

-- Each type requires two instances.

-- 1. Matching - query type equals result type

-- 2. Default - query type does not equal result type

-- These types (mostly primitive) are the leaves of datatypes, so their default
-- instances need to stop the recursion. Since no other type is found in them
-- type, we use 'collectempty' for the default instance.

instance Collect Char Char where collect = pure
instance Collect Char b    where collect = collectempty

instance Collect Int Int where collect = pure
instance Collect Int b   where collect = collectempty

instance Collect Float Float where collect = pure
instance Collect Float b     where collect = collectempty

instance Collect Double Double where collect = pure
instance Collect Double b      where collect = collectempty

instance Collect Bool Bool where collect = pure
instance Collect Bool b    where collect = collectempty

instance Collect () () where collect = pure
instance Collect () b  where collect = collectempty

-- For all other types, the default instance is standard.

instance Collect (Maybe b) (Maybe b) where collect = pure
instance Collect b a => Collect (Maybe b) a

instance Collect [b] [b] where collect = pure
instance Collect b a => Collect [b] a

instance Collect (b,c) (b,c) where collect = pure
instance (Collect b a, Collect c a) => Collect (b,c) a

instance Collect (b,c,d) (b,c,d) where collect = pure
instance (Collect b a, Collect c a, Collect d a) => Collect (b,c,d) a

instance Collect (b,c,d,e) (b,c,d,e) where collect = pure
instance (Collect b a, Collect c a, Collect d a, Collect e a) => Collect (b,c,d,e) a

instance Collect (b,c,d,e,f) (b,c,d,e,f) where collect = pure
instance (Collect b a, Collect c a, Collect d a, Collect e a, Collect f a) => Collect (b,c,d,e,f) a

instance Collect (b,c,d,e,f,g) (b,c,d,e,f,g) where collect = pure
instance (Collect b a, Collect c a, Collect d a, Collect e a, Collect f a, Collect g a) => Collect (b,c,d,e,f,g) a

instance Collect (b,c,d,e,f,g,h) (b,c,d,e,f,g,h) where collect = pure
instance (Collect b a, Collect c a, Collect d a, Collect e a, Collect f a, Collect g a, Collect h a) => Collect (b,c,d,e,f,g,h) a

