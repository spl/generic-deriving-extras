{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}

module Generics.Deriving.Query (
  Q,
  Query(..),
  mquery,
  collect,
  querydefault,
  queryapply,
  queryempty,
) where

--------------------------------------------------------------------------------

import GHC.Generics
import Data.Monoid (Monoid(mempty, mappend))
import Control.Applicative (pure, Alternative(empty, (<|>)))

--------------------------------------------------------------------------------

type Q c a b = b -> (b -> b -> b) -> (a -> b) -> c -> b

-- | The non-exported type-indexed function

class Query' f a where
  query' :: Q (f x) a b

instance Query' U1 a where
  query' e _ _ U1 = e

instance Query b a => Query' (K1 i b) a where
  query' e p f (K1 x) = query e p f x

instance Query' f a => Query' (M1 i c f) a where
  query' e p f (M1 x) = query' e p f x

instance (Query' f a, Query' h a) => Query' (f :+: h) a where
  query' e p f (L1 x) = query' e p f x
  query' e p f (R1 x) = query' e p f x

instance (Query' f a, Query' h a) => Query' (f :*: h) a where
  query' e p f (x :*: y) = query' e p f x `p` query' e p f y

--------------------------------------------------------------------------------

querydefault :: (Generic c, Query' (Rep c) a) => Q c a b
querydefault e p f = query' e p f . from

queryapply :: Q a a b
queryapply _ _ f = f

queryempty :: Q c a b
queryempty e _ _ _ = e

--------------------------------------------------------------------------------

class Query c a where
  query :: Q c a b

  default query :: (Generic c, Query' (Rep c) a) => Q c a b
  query = querydefault

--------------------------------------------------------------------------------

-- Each type requires two instances.

-- 1. Matching - query type equals result type

-- 2. Default - query type does not equal result type

-- These types (mostly primitive) are the leaves of datatypes, so their default
-- instances need to stop the recursion. Since no other type is found in them
-- type, we use 'collectempty' for the default instance.

instance Query Char Char where query = queryapply
instance Query Char b    where query = queryempty

instance Query Int Int where query = queryapply
instance Query Int b   where query = queryempty

instance Query Float Float where query = queryapply
instance Query Float b     where query = queryempty

instance Query Double Double where query = queryapply
instance Query Double b      where query = queryempty

instance Query Bool Bool where query = queryapply
instance Query Bool b    where query = queryempty

instance Query () () where query = queryapply
instance Query () b  where query = queryempty

-- For all other types, the default instance is standard.

instance Query (Maybe b) (Maybe b) where query = queryapply
instance Query b a => Query (Maybe b) a

instance Query [b] [b] where query = queryapply
instance Query b a => Query [b] a

instance Query (b,c) (b,c) where query = queryapply
instance (Query b a, Query c a) => Query (b,c) a

instance Query (b,c,d) (b,c,d) where query = queryapply
instance (Query b a, Query c a, Query d a) => Query (b,c,d) a

instance Query (b,c,d,e) (b,c,d,e) where query = queryapply
instance (Query b a, Query c a, Query d a, Query e a) => Query (b,c,d,e) a

instance Query (b,c,d,e,f) (b,c,d,e,f) where query = queryapply
instance (Query b a, Query c a, Query d a, Query e a, Query f a) => Query (b,c,d,e,f) a

instance Query (b,c,d,e,f,g) (b,c,d,e,f,g) where query = queryapply
instance (Query b a, Query c a, Query d a, Query e a, Query f a, Query g a) => Query (b,c,d,e,f,g) a

instance Query (b,c,d,e,f,g,h) (b,c,d,e,f,g,h) where query = queryapply
instance (Query b a, Query c a, Query d a, Query e a, Query f a, Query g a, Query h a) => Query (b,c,d,e,f,g,h) a

--------------------------------------------------------------------------------

mquery :: (Query c a, Monoid b) => (a -> b) -> c -> b
mquery = query mempty mappend

collect :: (Query c a, Alternative f) => c -> f a
collect = query empty (<|>) pure

--------------------------------------------------------------------------------

