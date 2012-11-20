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

instance Collect' (K1 i a) a where
  collect' (K1 x) = pure x

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

--------------------------------------------------------------------------------

class Collect b a where
  collect :: Alternative f => b -> f a

  default collect :: (Generic b, Collect' (Rep b) a, Alternative f) => b -> f a
  collect = collectdefault

-- Primitives
instance Collect Char   Char   where collect = pure
instance Collect Int    Int    where collect = pure
instance Collect Float  Float  where collect = pure
instance Collect Bool   Bool   where collect = pure

-- Type constructors: two cases needed
-- 1. Type equality: pure
instance Collect (Maybe a) (Maybe a) where collect = pure
instance Collect [a]       [a]       where collect = pure

-- 2. Default case for that type
instance Collect b a => Collect (Maybe b) a
instance Collect b a => Collect [b] a

-- Overall default failure case
instance Collect b a where collect _ = empty

