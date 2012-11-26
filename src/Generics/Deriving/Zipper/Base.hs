{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}

--------------------------------------------------------------------------------

module Generics.Deriving.Zipper.Base (
  Zipper(..),
  Dir(..),
  -- *
  enter,
  leave,
  -- *
  up,
  down,
  move,
  -- *
  get,
  set,
  -- *
  Loc,
  Empty,
  (:<:),
  -- *
  module Data.Typeable,
) where

--------------------------------------------------------------------------------

import Control.Arrow (second)
import Control.Applicative ((<$>), (<*>), (<|>))
import Data.Maybe (fromJust)

import Data.Typeable

import Generics.Deriving

--------------------------------------------------------------------------------

data family Ctx (f :: * -> *) p :: *

data instance Ctx U1 p

data instance Ctx (K1 i a) p = CK

newtype instance Ctx (M1 i c f) p = CM (Ctx f p)

data instance Ctx (f :+: g) p = CL (Ctx f p)
                              | CR (Ctx g p)

data instance Ctx (f :*: g) p = C1 (g p) (Ctx f p)
                              | C2 (f p) (Ctx g p)

--------------------------------------------------------------------------------

data Dir = L | R deriving (Show, Eq)

--------------------------------------------------------------------------------

class Zipper' f where
  fill  :: Typeable a => Ctx f p -> a -> Maybe (f p)
  split :: Typeable a => Dir -> f p -> Maybe (a, Ctx f p)
  creep :: (Typeable a, Typeable b) => Dir -> Ctx f p -> a -> Maybe (b, Ctx f p)

instance Zipper' U1 where
  fill = error "impossible"
  split _ _ = Nothing
  creep _ _ _ = Nothing

instance Typeable a => Zipper' (K1 i a) where
  fill CK x = K1 <$> cast x
  split _ (K1 x) = flip (,) CK <$> cast x
  creep _ _ _ = Nothing

instance Zipper' f => Zipper' (M1 i c f) where
  fill (CM c) x = M1 <$> fill c x
  split dir (M1 x) = fmap CM <$> split dir x
  creep dir (CM c) x = fmap CM <$> creep dir c x

instance (Zipper' f, Zipper' g) => Zipper' (f :+: g) where
  fill (CL l) x = L1 <$> fill l x
  fill (CR r) x = R1 <$> fill r x
  split dir (L1 x) = fmap CL <$> split dir x
  split dir (R1 x) = fmap CR <$> split dir x
  creep dir (CL c) x = fmap CL <$> creep dir c x
  creep dir (CR c) y = fmap CR <$> creep dir c y

instance (Zipper' f, Zipper' g) => Zipper' (f :*: g) where
  fill (C1 y c) x = (:*: y) <$> fill c x
  fill (C2 x c) y = (x :*:) <$> fill c y
  split dir (x :*: y) =
    choose dir (fmap (C1 y) <$> split dir x)
               (fmap (C2 x) <$> split dir y)
    where
      choose L = (<|>)
      choose R = flip (<|>)
  creep dir (C1 y c) x =
    choose dir (fmap (C1 y) <$> creep dir c x)
               (second . C2 <$> fill c x <*> split L y)
    where
      choose L = const
      choose R = (<|>)
  creep dir (C2 x c) y =
    choose dir (fmap (C2 x) <$> creep dir c y)
               (second . C1 <$> fill c y <*> split R x)
    where
      choose L = (<|>)
      choose R = const

--------------------------------------------------------------------------------

data Empty
data c :<: cs

infixr 5 :<:

data Contexts hole root tail where
  CNil   :: Contexts hole hole Empty
  CCons  :: Zipper parent
         => Ctx (Rep parent) p
         -> Contexts parent root cs
         -> Contexts hole root (parent :<: cs)

data Loc foc root c = Loc foc (Contexts foc root c)

fromOne :: Generic a => Contexts a r c -> Rep a x -> Loc a r c
fromOne cs foc = Loc (to foc) cs

fromPair :: Zipper a => Contexts a r cs -> (foc, Ctx (Rep a) p) -> Loc foc r (a :<: cs)
fromPair cs (foc, c) = Loc foc (CCons c cs)

--------------------------------------------------------------------------------

class (Generic a, Zipper' (Rep a), Typeable a) => Zipper a

--------------------------------------------------------------------------------

up :: (Zipper a, Zipper b) => Loc a r (b :<: c) -> Loc b r c
up (Loc foc (CCons c cs)) = fromJust (fromOne cs <$> fill c foc)

down :: (Zipper a, Zipper b) => Dir -> Loc a r c -> Maybe (Loc b r (a :<: c))
down dir (Loc h cs) = fromPair cs <$> split dir (from h)

move :: (Zipper a, Zipper b) => Dir -> Loc a r (c :<: cs) -> Maybe (Loc b r (c :<: cs))
move dir (Loc h (CCons c cs)) = fromPair cs <$> creep dir c h

--------------------------------------------------------------------------------

enter :: Zipper a => a -> Loc a a Empty
enter foc = Loc foc CNil

leave :: Zipper a => Loc a r c -> r
leave (Loc f CNil) = f
leave loc@(Loc _ (CCons {})) = leave (up loc)

--------------------------------------------------------------------------------

get :: Loc a r c -> a
get (Loc foc _) = foc

set :: a -> Loc a r c -> Loc a r c
set foc (Loc _ ctxs) = Loc foc ctxs

--------------------------------------------------------------------------------

instance Zipper Bool
instance Zipper Char
instance Zipper Double
instance Zipper Float
instance Zipper Int
instance Zipper Ordering

instance Typeable a => Zipper [a]
instance Typeable a => Zipper (Maybe a)
instance (Typeable a, Typeable b) => Zipper (Either a b)

instance Zipper ()
instance (Typeable a, Typeable b) => Zipper (a, b)
instance (Typeable a, Typeable b, Typeable c) => Zipper (a, b, c)
instance (Typeable a, Typeable b, Typeable c, Typeable d) => Zipper (a, b, c, d)
instance (Typeable a, Typeable b, Typeable c, Typeable d, Typeable e) => Zipper (a, b, c, d, e)
instance (Typeable a, Typeable b, Typeable c, Typeable d, Typeable e, Typeable f) => Zipper (a, b, c, d, e, f)
instance (Typeable a, Typeable b, Typeable c, Typeable d, Typeable e, Typeable f, Typeable g) => Zipper (a, b, c, d, e, f, g)

