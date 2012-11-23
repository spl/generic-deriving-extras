{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}

--------------------------------------------------------------------------------

module Generics.Instant.Zipper (
  Zipper(..),
  enter,
  leave,
  first,
  last,
  left,
  right,
  get,
  set,
  -- *
  Loc,
  Empty,
  (:<:),
  Pos,
  Dir,
  -- *
  module Data.Typeable,
) where

--------------------------------------------------------------------------------

import Prelude hiding (last)

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

type Choice a = Maybe a -> Maybe a -> Maybe a

type Pos = forall a . Choice a

first :: Pos
first = (<|>)

last :: Pos
last = flip (<|>)

type Dir = forall a . (Choice a, Choice a)

right :: Dir
right = ((<|>), const)

left :: Dir
left = (const, (<|>))

--------------------------------------------------------------------------------

class Zipper' f where
  fill  :: Typeable a => Ctx f p -> a -> Maybe (f p)
  split :: Typeable a => Pos -> f p -> Maybe (a, Ctx f p)
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
  split pos (M1 x) = fmap CM <$> split pos x
  creep dir (CM c) x = fmap CM <$> creep dir c x

instance (Zipper' f, Zipper' g) => Zipper' (f :+: g) where
  fill (CL l) x = L1 <$> fill l x
  fill (CR r) x = R1 <$> fill r x
  split pos (L1 x) = fmap CL <$> split pos x
  split pos (R1 x) = fmap CR <$> split pos x
  creep dir (CL c) x = fmap CL <$> creep dir c x
  creep dir (CR c) y = fmap CR <$> creep dir c y

instance (Zipper' f, Zipper' g) => Zipper' (f :*: g) where
  fill (C1 y c) x = (:*: y) <$> fill c x
  fill (C2 x c) y = (x :*:) <$> fill c y
  split pos (x :*: y) = pos (fmap (C1 y) <$> split pos x)
                             (fmap (C2 x) <$> split pos y)
  creep dir (C1 y c) x = fst dir (fmap (C1 y) <$> creep dir c x)
                                 (second . C2 <$> fill c x <*> split first y)
  creep dir (C2 x c) y = snd dir (fmap (C2 x) <$> creep dir c y)
                                 (second . C1 <$> fill c y <*> split last x)

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

-- | Navigation

get :: Loc a r c -> a
get (Loc foc _) = foc

set :: a -> Loc a r c -> Loc a r c
set foc (Loc _ ctxs) = Loc foc ctxs

enter :: Zipper a => a -> Loc a a Empty
enter foc = Loc foc CNil

leave :: Zipper a => Loc a r c -> r
leave (Loc f CNil) = f
leave loc@(Loc _ (CCons {})) = leave (up loc)

class (Generic a, Zipper' (Rep a), Typeable a) => Zipper a where

  up :: Zipper b => Loc a r (b :<: c) -> Loc b r c
  up (Loc foc (CCons c cs)) = fromJust (fromOne cs <$> fill c foc)

  down :: Zipper b => Pos -> Loc a r c -> Maybe (Loc b r (a :<: c))
  down pos (Loc h cs) = fromPair cs <$> split pos (from h)

  move :: Zipper b => Dir -> Loc a r (c :<: cs) -> Maybe (Loc b r (c :<: cs))
  move dir (Loc h (CCons c cs)) = fromPair cs <$> creep dir c h

