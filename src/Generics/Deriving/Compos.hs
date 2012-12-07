{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DefaultSignatures #-}

--------------------------------------------------------------------------------

module Generics.Deriving.Compos (
  Compos(..),
  composOp,
  composFold,
  composM,
  composM_,
) where

--------------------------------------------------------------------------------

import GHC.Generics

import Control.Applicative ((<$>), Applicative(..), WrappedMonad(..))
import Control.Monad (liftM)
import Data.Monoid (Monoid(..))
import Data.Functor.Identity (Identity(..))
import Data.Functor.Constant (Constant(..))

--------------------------------------------------------------------------------

class Compos' f a where
  compos' :: Applicative m => (a -> m a) -> f x -> m (f x)

--------------------------------------------------------------------------------

instance Compos' U1 a where
  compos' f U1 = pure U1

instance Compos a => Compos' (K1 i a) a where
  compos' f (K1 x) = K1 <$> f x

instance Compos' (K1 i b) a where
  compos' f (K1 x) = pure (K1 x)

instance Compos' f a => Compos' (M1 i c f) a where
  compos' f (M1 x) = M1 <$> compos' f x

instance (Compos' f a, Compos' g a) => Compos' (f :+: g) a where
  compos' f (L1 x) = L1 <$> compos' f x
  compos' f (R1 x) = R1 <$> compos' f x

instance (Compos' f a, Compos' g a) => Compos' (f :*: g) a where
  compos' f (x :*: y) = (:*:) <$> compos' f x <*> compos' f y

--------------------------------------------------------------------------------

class Compos a where
  compos :: Applicative m => (a -> m a) -> a -> m a

  default compos :: (Applicative m, Generic a, Compos' (Rep a) a)
                 => (a -> m a) -> a -> m a
  compos f = fmap to . compos' f . from

--------------------------------------------------------------------------------

composOp :: Compos a => (a -> a) -> a -> a
composOp f = runIdentity . compos (Identity . f)

composFold :: (Monoid b, Compos a) => (a -> b) -> a -> b
composFold f = getConstant . compos (Constant . f)

composM :: (Monad m, Compos a) => (a -> m a) -> a -> m a
composM f = unwrapMonad . compos (WrapMonad . f)

composM_ :: (Monad m, Compos a) => (a -> m ()) -> a -> m ()
composM_ f = unwrapMonad_ . composFold (WrapMonad_ . f)

--------------------------------------------------------------------------------

newtype WrappedMonad_ m = WrapMonad_ { unwrapMonad_ :: m () }

instance Monad m => Monoid (WrappedMonad_ m) where
  mempty                              = WrapMonad_ (return ())
  WrapMonad_ x `mappend` WrapMonad_ y = WrapMonad_ (x >> y)

--------------------------------------------------------------------------------

instance Compos Bool
instance Compos Char
instance Compos Double
instance Compos Float
instance Compos Int
instance Compos Ordering

instance Compos [a]
instance Compos (Maybe a)
instance Compos (Either a b)

instance Compos ()
instance Compos (a, b)
instance Compos (a, b, c)
instance Compos (a, b, c, d)
instance Compos (a, b, c, d, e)
instance Compos (a, b, c, d, e, f)
instance Compos (a, b, c, d, e, f, g)

