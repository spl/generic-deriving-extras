{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}

--------------------------------------------------------------------------------

module Generics.Deriving.Zipper.Base (
  Zipper(..),
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
  -- *
  Loc,
  -- *
  Generic,
  Typeable,
) where

--------------------------------------------------------------------------------

import GHC.Generics
import Generics.Deriving.Util (Dir(..), dir)
import Generics.Deriving.Zipper.Context

import Data.Typeable (Typeable, cast)

import Control.Arrow (second)
import Control.Applicative ((<$>), (<*>), (<|>))
import Data.Maybe (fromJust)

--------------------------------------------------------------------------------

class Zipper' f where
  fill  :: Typeable a => Ctx f p -> a -> Maybe (f p)
  split :: Typeable a => Dir -> f p -> Maybe (a, Ctx f p)
  shift :: (Typeable a, Typeable b) => Dir -> Ctx f p -> a -> Maybe (b, Ctx f p)

instance Zipper' U1 where
  fill = error "impossible"
  split _ _ = Nothing
  shift _ _ _ = Nothing

instance Typeable a => Zipper' (K1 i a) where
  fill CK x = K1 <$> cast x
  split _ (K1 x) = flip (,) CK <$> cast x
  shift _ _ _ = Nothing

instance Zipper' f => Zipper' (M1 i c f) where
  fill (CM c) x = M1 <$> fill c x
  split d (M1 x) = second CM <$> split d x
  shift d (CM c) x = second CM <$> shift d c x

instance (Zipper' f, Zipper' g) => Zipper' (f :+: g) where
  fill (CL l) x = L1 <$> fill l x
  fill (CR r) x = R1 <$> fill r x
  split d (L1 x) = second CL <$> split d x
  split d (R1 x) = second CR <$> split d x
  shift d (CL c) x = second CL <$> shift d c x
  shift d (CR c) y = second CR <$> shift d c y

instance (Zipper' f, Zipper' g) => Zipper' (f :*: g) where
  fill (C1 y c) x = (:*: y) <$> fill c x
  fill (C2 x c) y = (x :*:) <$> fill c y
  split d (x :*: y) =
    dir d (<|>) (flip (<|>)) (second (C1 y) <$> split d x)
                             (second (C2 x) <$> split d y)
  shift d (C1 y c) x =
    dir d const (<|>) (second (C1 y) <$> shift d c x)
                      (second . C2 <$> fill c x <*> split L y)
  shift d (C2 x c) y =
    dir d (<|>) const (second (C2 x) <$> shift d c y)
                      (second . C1 <$> fill c y <*> split R x)

--------------------------------------------------------------------------------

data Contexts hole root tail where
  CNil   :: Contexts hole hole '[]
  CCons  :: Zipper parent
         => Ctx (Rep parent) p
         -> Contexts parent root cs
         -> Contexts hole root (parent ': cs)

data Loc a root c = Loc a (Contexts a root c)

fromOne :: Generic a => Contexts a r c -> Rep a x -> Loc a r c
fromOne cs x = Loc (to x) cs

fromPair :: Zipper b => Contexts b r cs -> (a, Ctx (Rep b) p) -> Loc a r (b ': cs)
fromPair cs (x, c) = Loc x (CCons c cs)

--------------------------------------------------------------------------------

class (Generic a, Zipper' (Rep a), Typeable a) => Zipper a

--------------------------------------------------------------------------------

up :: (Zipper a, Zipper b) => Loc a r (b ': c) -> Loc b r c
up (Loc x (CCons c cs)) = fromJust (fromOne cs <$> fill c x)

upM :: (Monad m, Zipper a, Zipper b) => Loc a r (b ': c) -> m (Loc b r c)
upM = return . up

down :: (Zipper a, Zipper b) => Dir -> Loc a r c -> Maybe (Loc b r (a ': c))
down d (Loc h cs) = fromPair cs <$> split d (from h)

move :: (Zipper a, Zipper b) => Dir -> Loc a r (c ': cs) -> Maybe (Loc b r (c ': cs))
move d (Loc h (CCons c cs)) = fromPair cs <$> shift d c h

--------------------------------------------------------------------------------

enter :: Zipper a => a -> Loc a a '[]
enter x = Loc x CNil

leave :: Zipper a => Loc a r c -> r
leave (Loc x CNil)           = x
leave loc@(Loc _ (CCons {})) = leave (up loc)

leaveM :: (Monad m, Zipper a) => Loc a r c -> m r
leaveM = return . leave

--------------------------------------------------------------------------------

get :: Loc a r c -> a
get (Loc x _) = x

getM :: Monad m => Loc a r c -> m a
getM = return . get

set :: a -> Loc a r c -> Loc a r c
set x (Loc _ cs) = Loc x cs

setM :: Monad m => a -> Loc a r c -> m (Loc a r c)
setM x = return . set x

modify :: (a -> a) -> Loc a r c -> Loc a r c
modify f (Loc x cs) = Loc (f x) cs

modifyM :: Monad m => (a -> a) -> Loc a r c -> m (Loc a r c)
modifyM f = return . modify f

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

