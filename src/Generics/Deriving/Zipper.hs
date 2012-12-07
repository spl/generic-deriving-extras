{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

--------------------------------------------------------------------------------

module Generics.Deriving.Zipper (
  Zipper(..),
  Loc,
  Dir(..),
  -- *
  enter,
  leave,
  leaveM,
  -- *
  up,
  upM,
  down,
  move,
  -- *
  get,
  getM,
  set,
  setM,
  modify,
  modifyM,
) where

--------------------------------------------------------------------------------

import GHC.Generics
import Generics.Deriving.Util (Dir(..), dir)
import Generics.Deriving.Zipper.Context

import Control.Arrow (second)
import Control.Applicative ((<$>), (<*>), (<|>))
import Data.Maybe (fromJust)

--------------------------------------------------------------------------------

class Zipper' f a where
  fill  :: Ctx f x -> a -> Maybe (f x)
  split :: Dir -> f x -> Maybe (a, Ctx f x)
  shift :: Dir -> Ctx f x -> a -> Maybe (a, Ctx f x)

instance Zipper' U1 a where
  fill = error "impossible"
  split _ _ = Nothing
  shift _ _ _ = Nothing

instance Zipper' (K1 i a) a where
  fill CK x = Just (K1 x)
  split _ (K1 x) = Just (x, CK)
  shift _ _ _ = Nothing

instance Zipper' (K1 i a) b where
  fill CK x = Nothing
  split _ (K1 x) = Nothing
  shift _ _ _ = Nothing

instance Zipper' f a => Zipper' (M1 i c f) a where
  fill (CM c) x = M1 <$> fill c x
  split d (M1 x) = fmap CM <$> split d x
  shift d (CM c) x = fmap CM <$> shift d c x

instance (Zipper' f a, Zipper' g a) => Zipper' (f :+: g) a where
  fill (CL l) x = L1 <$> fill l x
  fill (CR r) x = R1 <$> fill r x
  split d (L1 x) = fmap CL <$> split d x
  split d (R1 x) = fmap CR <$> split d x
  shift d (CL c) x = fmap CL <$> shift d c x
  shift d (CR c) y = fmap CR <$> shift d c y

instance (Zipper' f a, Zipper' g a) => Zipper' (f :*: g) a where
  fill (C1 y c) x = (:*: y) <$> fill c x
  fill (C2 x c) y = (x :*:) <$> fill c y
  split d (x :*: y) =
    dir d (<|>) (flip (<|>)) (fmap (C1 y) <$> split d x)
                             (fmap (C2 x) <$> split d y)
  shift d (C1 y c) x =
    dir d const (<|>) (fmap (C1 y) <$> shift d c x)
                      (second . C2 <$> fill c x <*> split L y)
  shift d (C2 x c) y =
    dir d (<|>) const (fmap (C2 x) <$> shift d c y)
                      (second . C1 <$> fill c y <*> split R x)

--------------------------------------------------------------------------------

data Loc a where
  Loc :: Zipper a => a -> [Ctx (Rep a) x] -> Loc a

--------------------------------------------------------------------------------

class (Generic a, Zipper' (Rep a) a) => Zipper a

up :: Loc a -> Loc a
up (Loc x (c:cs)) = Loc (to (fromJust (fill c x))) cs

upM :: Monad m => Loc a -> m (Loc a)
upM = return . up

down :: Dir -> Loc a -> Maybe (Loc a)
down d (Loc h cs)     = (\(x, c) -> Loc x (c:cs)) <$> split d (from h)

move :: Dir -> Loc a -> Maybe (Loc a)
move d (Loc h (c:cs)) = (\(x, c) -> Loc x (c:cs)) <$> shift d c h

--------------------------------------------------------------------------------

enter :: Zipper a => a -> Loc a
enter x = Loc x []

leave :: Loc a -> a
leave (Loc x []) = x
leave loc        = leave (up loc)

leaveM :: Monad m => Loc a -> m a
leaveM = return . leave

--------------------------------------------------------------------------------

get :: Loc a -> a
get (Loc x _) = x

getM :: Monad m => Loc a -> m a
getM = return . get

set :: a -> Loc a -> Loc a
set x (Loc _ cs) = Loc x cs

setM :: Monad m => a -> Loc a -> m (Loc a)
setM x = return . set x

modify :: (a -> a) -> Loc a -> Loc a
modify f (Loc x cs) = Loc (f x) cs

modifyM :: Monad m => (a -> a) -> Loc a -> m (Loc a)
modifyM f = return . modify f

--------------------------------------------------------------------------------

instance Zipper Bool
instance Zipper Char
instance Zipper Double
instance Zipper Float
instance Zipper Int
instance Zipper Ordering

instance Zipper [a]
instance Zipper (Maybe a)
instance Zipper (Either a b)

instance Zipper ()
instance Zipper (a, b)
instance Zipper (a, b, c)
instance Zipper (a, b, c, d)
instance Zipper (a, b, c, d, e)
instance Zipper (a, b, c, d, e, f)
instance Zipper (a, b, c, d, e, f, g)

