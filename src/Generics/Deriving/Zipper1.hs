{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

--------------------------------------------------------------------------------

module Generics.Deriving.Zipper1 (
  Zipper1(..),
  Loc1,
  Dir(..),
  -- *
  enter1,
  leave1,
  leave1M,
  -- *
  move1,
  -- *
  get1,
  get1M,
  set1,
  set1M,
  modify1,
  modify1M,
) where

--------------------------------------------------------------------------------

import GHC.Generics
import Generics.Deriving.Util (Dir(..), dir)
import Generics.Deriving.Zipper1.Context

import Control.Arrow (second)
import Control.Applicative ((<$>), (<*>), (<|>))
import Data.Maybe (fromJust)

--------------------------------------------------------------------------------

class Zipper1' f where
  fill  :: Ctx1 f p -> p -> Maybe (f p)
  split :: Dir -> f p -> Maybe (p, Ctx1 f p)
  shift :: Dir -> Ctx1 f p -> p -> Maybe (p, Ctx1 f p)

instance Zipper1' U1 where
  fill = error "impossible"
  split _ _ = Nothing
  shift _ _ _ = Nothing

instance Zipper1' (K1 i a) where
  fill _ _ = Nothing
  split _ _ = Nothing
  shift _ _ _ = Nothing

instance Zipper1' f => Zipper1' (M1 i c f) where
  fill (CM c) p = M1 <$> fill c p
  split d (M1 x) = second CM <$> split d x
  shift d (CM c) p = second CM <$> shift d c p

instance Zipper1' Par1 where
  fill CPar p = Just (Par1 p)
  split _ (Par1 x) = Just (x, CPar)
  shift _ _ _ = Nothing

instance Zipper1 f => Zipper1' (Rec1 f) where
  fill (CRec c) p = Rec1 . to1 <$> fill c p
  split d (Rec1 x) = second CRec <$> split d (from1 x)
  shift d (CRec c) p = second CRec <$> shift d c p

instance (Zipper1' f, Zipper1' g) => Zipper1' (f :+: g) where
  fill (CL l) p = L1 <$> fill l p
  fill (CR r) p = R1 <$> fill r p
  split d (L1 x) = second CL <$> split d x
  split d (R1 x) = second CR <$> split d x
  shift d (CL c) p = second CL <$> shift d c p
  shift d (CR c) p = second CR <$> shift d c p

instance (Zipper1' f, Zipper1' g) => Zipper1' (f :*: g) where
  fill (C1 y c) p = (:*: y) <$> fill c p
  fill (C2 x c) p = (x :*:) <$> fill c p
  split d (x :*: y) =
    dir d (<|>) (flip (<|>)) (second (C1 y) <$> split d x)
                             (second (C2 x) <$> split d y)
  shift d (C1 y c) p =
    dir d const (<|>) (second (C1 y) <$> shift d c p)
                      (second . C2 <$> fill c p <*> split L y)
  shift d (C2 x c) p =
    dir d (<|>) const (second (C2 x) <$> shift d c p)
                      (second . C1 <$> fill c p <*> split R x)

instance Zipper1' g => Zipper1' ([] :.: g) where
  fill (CCons1 xs c) p = Comp1 . (: xs) <$> fill c p
  fill (CCons2 x  c) p = (\(Comp1 xs) -> Comp1 (x:xs)) <$> fill c p
  split d (Comp1 [])     = Nothing
  split d (Comp1 (x:xs)) =
    dir d (<|>) (flip (<|>)) (second (CCons1 xs) <$> split d x)
                             (second (CCons2 x) <$> split d (Comp1 xs))
  shift d (CCons1 xs c) p =
    dir d const (<|>) (second (CCons1 xs) <$> shift d c p)
                      (second . CCons2 <$> fill c p <*> split L (Comp1 xs))
  shift d (CCons2 x c) p =
    dir d (<|>) const (second (CCons2 x) <$> shift d c p)
                      (second . CCons1 . unComp1 <$> fill c p <*> split R x)

instance Zipper1' g => Zipper1' (Maybe :.: g) where
  fill (CJust c) p = Comp1 . Just <$> fill c p
  split d (Comp1 Nothing)  = Nothing
  split d (Comp1 (Just x)) = second CJust <$> split d x
  shift d (CJust c) p = second CJust <$> shift d c p

instance Zipper1' g => Zipper1' (Either a :.: g) where
  fill (CRight c) p = Comp1 . Right <$> fill c p
  split d (Comp1 (Left _))  = Nothing
  split d (Comp1 (Right x)) = second CRight <$> split d x
  shift d (CRight c) p = second CRight <$> shift d c p

--------------------------------------------------------------------------------

class (Generic1 f, Zipper1' (Rep1 f)) => Zipper1 f

data Loc1 f a where
  Loc1 :: Zipper1 f => a -> Ctx1 (Rep1 f) a -> Loc1 f a

-- Remove this once representation types have Generic1 instances. Use GFunctor
-- instead.
instance Functor (Ctx1 (Rep1 f)) => Functor (Loc1 f) where
  fmap f (Loc1 p c) = Loc1 (f p) (fmap f c)

--------------------------------------------------------------------------------

enter1 :: Zipper1 f => Dir -> f a -> Maybe (Loc1 f a)
enter1 d x = uncurry Loc1 <$> split d (from1 x)

leave1 :: Loc1 f a -> f a
leave1 (Loc1 p c) = to1 (fromJust (fill c p))

leave1M :: Monad m => Loc1 f a -> m (f a)
leave1M = return . leave1

--------------------------------------------------------------------------------

move1 :: Dir -> Loc1 f a -> Maybe (Loc1 f a)
move1 d (Loc1 p c) = uncurry Loc1 <$> shift d c p

--------------------------------------------------------------------------------

get1 :: Loc1 f a -> a
get1 (Loc1 p _) = p

get1M :: Monad m => Loc1 f a -> m a
get1M = return . get1

set1 :: a -> Loc1 f a -> Loc1 f a
set1 p (Loc1 _ c) = Loc1 p c

set1M :: Monad m => a -> Loc1 f a -> m (Loc1 f a)
set1M p = return . set1 p

modify1 :: (a -> a) -> Loc1 f a -> Loc1 f a
modify1 f (Loc1 p c) = Loc1 (f p) c

modify1M :: Monad m => (a -> a) -> Loc1 f a -> m (Loc1 f a)
modify1M f = return . modify1 f

--------------------------------------------------------------------------------

instance Zipper1 []
instance Zipper1 Maybe
instance Zipper1 (Either a)

instance Zipper1 ((,) a)
instance Zipper1 ((,,) a b)
instance Zipper1 ((,,,) a b c)
instance Zipper1 ((,,,,) a b c d)
instance Zipper1 ((,,,,,) a b c d e)
instance Zipper1 ((,,,,,,) a b c d e f)

