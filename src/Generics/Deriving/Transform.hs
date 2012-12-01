{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}

--------------------------------------------------------------------------------

module Generics.Deriving.Transform (
  -- *
  T,
  TM,
  TA,
  Dir,
  -- *
  Transform(..),
  -- *
  -- **
  bottomUp_apply,
  topDown_apply,
  bottomUpM_apply,
  topDownM_apply,
  transform_apply,
  transformM_apply,
  transformA_apply,
  -- **
  bottomUp_apply1,
  topDown_apply1,
  bottomUpM_apply1,
  topDownM_apply1,
  transform_apply1,
  transformM_apply1,
  transformA_apply1,
  -- *
  -- **
  bottomUp_default,
  topDown_default,
  bottomUpM_default,
  topDownM_default,
  transform_default,
  transformM_default,
  transformA_default,
  -- **
  bottomUp_empty,
  topDown_empty,
  bottomUpM_empty,
  topDownM_empty,
  transform_empty,
  transformM_empty,
  transformA_empty,
) where

--------------------------------------------------------------------------------

import GHC.Generics
import Control.Arrow
import Control.Monad (liftM, liftM2, (<=<), (>=>))

--------------------------------------------------------------------------------

type T  b a =                             (a ->   a) -> b ->   b
type TM b a = forall m . Monad m       => (a -> m a) -> b -> m b
type TA b a = forall c . ArrowChoice c => c a a      -> c b b

data Dir = BU | TD

--------------------------------------------------------------------------------

class Transform' f a where
  bottomUp'  :: T  (f x) a
  topDown'   :: T  (f x) a
  bottomUpM' :: TM (f x) a
  topDownM'  :: TM (f x) a
  transform'  :: Dir -> T  (f x) a
  transformM' :: Dir -> TM (f x) a
  transformA' :: Dir -> TA (f x) a

instance Transform' U1 a where
  bottomUp'  _ U1 = U1
  topDown'   _ U1 = U1
  bottomUpM' _ U1 = return U1
  topDownM'  _ U1 = return U1
  transform'  _ _ = id
  transformM' _ _ = return
  transformA' _ _ = returnA

instance Transform b a => Transform' (K1 i b) a where
  bottomUp'  f (K1 x) = K1 (bottomUp f x)
  topDown'   f (K1 x) = K1 (topDown  f x)
  bottomUpM' f (K1 x) = liftM K1 (bottomUpM f x)
  topDownM'  f (K1 x) = liftM K1 (topDownM  f x)
  transform'  dir f (K1 x) = K1 (transform dir f x)
  transformM' dir f (K1 x) = liftM K1 (transformM dir f x)
  transformA' dir f        = arr (\(K1 x) -> x)
                             >>> transformA dir f
                             >>> arr K1

instance Transform' f a => Transform' (M1 i c f) a where
  bottomUp'  f (M1 x) = M1 (bottomUp' f x)
  topDown'   f (M1 x) = M1 (topDown'  f x)
  bottomUpM' f (M1 x) = liftM M1 (bottomUpM' f x)
  topDownM'  f (M1 x) = liftM M1 (topDownM'  f x)
  transform'  dir f (M1 x) = M1 (transform' dir f x)
  transformM' dir f (M1 x) = liftM M1 (transformM' dir f x)
  transformA' dir f        = arr (\(M1 x) -> x)
                             >>> transformA' dir f
                             >>> arr M1

instance (Transform' f a, Transform' g a) => Transform' (f :+: g) a where
  bottomUp'  f (L1 x) = L1 (bottomUp' f x)
  bottomUp'  f (R1 x) = R1 (bottomUp' f x)
  topDown'   f (L1 x) = L1 (topDown'  f x)
  topDown'   f (R1 x) = R1 (topDown'  f x)
  bottomUpM' f (L1 x) = liftM L1 (bottomUpM' f x)
  bottomUpM' f (R1 x) = liftM R1 (bottomUpM' f x)
  topDownM'  f (L1 x) = liftM L1 (topDownM'  f x)
  topDownM'  f (R1 x) = liftM R1 (topDownM'  f x)
  transform'  dir f (L1 x) = L1 (transform' dir f x)
  transform'  dir f (R1 x) = R1 (transform' dir f x)
  transformM' dir f (L1 x) = liftM L1 (transformM' dir f x)
  transformM' dir f (R1 x) = liftM R1 (transformM' dir f x)
  transformA' dir f        = arr toEither
                             >>> transformA' dir f +++ transformA' dir f
                             >>> arr fromEither
    where
      fromEither (Left x)  = L1 x
      fromEither (Right x) = R1 x
      toEither (L1 x) = Left x
      toEither (R1 x) = Right x

instance (Transform' f a, Transform' g a) => Transform' (f :*: g) a where
  bottomUp'  f (x :*: y) = bottomUp' f x :*: bottomUp' f y
  topDown'   f (x :*: y) = topDown'  f x :*: topDown'  f y
  bottomUpM' f (x :*: y) = liftM2 (:*:) (bottomUpM' f x) (bottomUpM' f y)
  topDownM'  f (x :*: y) = liftM2 (:*:) (topDownM'  f x) (topDownM'  f y)
  transform'  dir f (x :*: y) = transform' dir f x :*: transform' dir f y
  transformM' dir f (x :*: y) = liftM2 (:*:) (transformM' dir f x) (transformM' dir f y)
  transformA' dir f           = arr (\(x :*: y) -> (x, y))
                                >>> transformA' dir f *** transformA' dir f
                                >>> arr (\(x, y) -> x :*: y)

--------------------------------------------------------------------------------

bottomUp_apply, topDown_apply :: (Generic a, Transform' (Rep a) a) => T a a
bottomUp_apply f = f . bottomUp_default f
topDown_apply  f = topDown_default f . f

bottomUpM_apply, topDownM_apply :: (Generic a, Transform' (Rep a) a) => TM a a
bottomUpM_apply f = f <=< bottomUpM_default f
topDownM_apply  f = topDownM_default f <=< f

transform_apply :: (Generic a, Transform' (Rep a) a) => Dir -> T a a
transform_apply dir f = f `compose` transform_default dir f
  where
    compose = case dir of
      BU -> (<<<)
      TD -> (>>>)

transformM_apply :: (Generic a, Transform' (Rep a) a) => Dir -> TM a a
transformM_apply dir f = f `compose` transformM_default dir f
  where
    compose = case dir of
      BU -> (<=<)
      TD -> (>=>)

transformA_apply :: (Generic a, Transform' (Rep a) a) => Dir -> TA a a
transformA_apply dir f = f `compose` transformA_default dir f
  where
    compose = case dir of
      BU -> (<<<)
      TD -> (>>>)

--------------------------------------------------------------------------------

bottomUp_apply1, topDown_apply1 :: T a a
bottomUp_apply1 = id
topDown_apply1  = id

bottomUpM_apply1, topDownM_apply1 :: TM a a
bottomUpM_apply1 = id
topDownM_apply1  = id

transform_apply1 :: Dir -> T a a
transform_apply1 _ = id

transformM_apply1 :: Dir -> TM a a
transformM_apply1 _ = id

transformA_apply1 :: Dir -> TA a a
transformA_apply1 _ = id

--------------------------------------------------------------------------------

bottomUp_default, topDown_default :: (Generic b, Transform' (Rep b) a) => T b a
bottomUp_default f = to . bottomUp' f . from
topDown_default  f = to . topDown'  f . from

bottomUpM_default, topDownM_default :: (Generic b, Transform' (Rep b) a) => TM b a
bottomUpM_default f = liftM to . bottomUpM' f . from
topDownM_default  f = liftM to . topDownM'  f . from

transform_default :: (Generic b, Transform' (Rep b) a) => Dir -> T b a
transform_default dir f = to . transform' dir f . from

transformM_default :: (Generic b, Transform' (Rep b) a) => Dir -> TM b a
transformM_default dir f = liftM to . transformM' dir f . from

transformA_default :: (Generic b, Transform' (Rep b) a) => Dir -> TA b a
transformA_default dir f = arr to <<< transformA' dir f <<< arr from

--------------------------------------------------------------------------------

bottomUp_empty, topDown_empty :: T b a
bottomUp_empty _ = id
topDown_empty  _ = id

bottomUpM_empty, topDownM_empty :: TM b a
bottomUpM_empty _ = return
topDownM_empty  _ = return

transform_empty :: Dir -> T b a
transform_empty _ _ = id

transformM_empty :: Dir -> TM b a
transformM_empty _ _ = return

transformA_empty :: Dir -> TA b a
transformA_empty _ _ = returnA

--------------------------------------------------------------------------------

class TransformAlt b a where
  bottomUpAlt  :: T  b a
  topDownAlt   :: T  b a
  bottomUpMAlt :: TM b a
  topDownMAlt  :: TM b a
  transformAlt  :: Dir -> T  b a
  transformMAlt :: Dir -> TM b a
  transformAAlt :: Dir -> TA b a

instance (Generic a, Transform' (Rep a) a) => TransformAlt a a where
  bottomUpAlt  = bottomUp_apply
  topDownAlt   = topDown_apply
  bottomUpMAlt = bottomUpM_apply
  topDownMAlt  = topDownM_apply
  transformAlt  = transform_apply
  transformMAlt = transformM_apply
  transformAAlt = transformA_apply

instance (Generic b, Transform' (Rep b) a) => TransformAlt b a where
  bottomUpAlt  = bottomUp_default
  topDownAlt   = topDown_default
  bottomUpMAlt = bottomUpM_default
  topDownMAlt  = topDownM_default
  transformAlt  = transform_default
  transformMAlt = transformM_default
  transformAAlt = transformA_default

--------------------------------------------------------------------------------

class Transform b a where

  bottomUp :: T b a

  default bottomUp :: TransformAlt b a => T b a
  bottomUp = bottomUpAlt

  topDown :: T b a

  default topDown :: TransformAlt b a => T b a
  topDown = topDownAlt

  bottomUpM :: TM b a

  default bottomUpM :: TransformAlt b a => TM b a
  bottomUpM = bottomUpMAlt

  topDownM :: TM b a

  default topDownM :: TransformAlt b a => TM b a
  topDownM = topDownMAlt

  transform  :: Dir -> T  b a
  transformM :: Dir -> TM b a
  transformA :: Dir -> TA b a

  default transform  :: TransformAlt b a => Dir -> T  b a
  default transformM :: TransformAlt b a => Dir -> TM b a
  default transformA :: TransformAlt b a => Dir -> TA b a

  transform  = transformAlt
  transformM = transformMAlt
  transformA = transformAAlt

--------------------------------------------------------------------------------

-- Primitive types:
-- * Must use _apply1 in the matching instance
-- * May use _empty in the fall-through instance

instance Transform Char Char where
  bottomUp  = bottomUp_apply1
  topDown   = topDown_apply1
  bottomUpM = bottomUpM_apply1
  topDownM  = topDownM_apply1
  transform  = transform_apply1
  transformM = transformM_apply1
  transformA = transformA_apply1
instance Transform Char a where
  bottomUp  = bottomUp_empty
  topDown   = topDown_empty
  bottomUpM = bottomUpM_empty
  topDownM  = topDownM_empty
  transform  = transform_empty
  transformM = transformM_empty
  transformA = transformA_empty

instance Transform Float Float where
  bottomUp  = bottomUp_apply1
  topDown   = topDown_apply1
  bottomUpM = bottomUpM_apply1
  topDownM  = topDownM_apply1
  transform  = transform_apply1
  transformM = transformM_apply1
  transformA = transformA_apply1
instance Transform Float a where
  bottomUp  = bottomUp_empty
  topDown   = topDown_empty
  bottomUpM = bottomUpM_empty
  topDownM  = topDownM_empty
  transform  = transform_empty
  transformM = transformM_empty
  transformA = transformA_empty

instance Transform Double Double where
  bottomUp  = bottomUp_apply1
  topDown   = topDown_apply1
  bottomUpM = bottomUpM_apply1
  topDownM  = topDownM_apply1
  transform  = transform_apply1
  transformM = transformM_apply1
  transformA = transformA_apply1
instance Transform Double a where
  bottomUp  = bottomUp_empty
  topDown   = topDown_empty
  bottomUpM = bottomUpM_empty
  topDownM  = topDownM_empty
  transform  = transform_empty
  transformM = transformM_empty
  transformA = transformA_empty

instance Transform Int Int where
  bottomUp  = bottomUp_apply1
  topDown   = topDown_apply1
  bottomUpM = bottomUpM_apply1
  topDownM  = topDownM_apply1
  transform  = transform_apply1
  transformM = transformM_apply1
  transformA = transformA_apply1
instance Transform Int a where
  bottomUp  = bottomUp_empty
  topDown   = topDown_empty
  bottomUpM = bottomUpM_empty
  topDownM  = topDownM_empty
  transform  = transform_empty
  transformM = transformM_empty
  transformA = transformA_empty

--------------------------------------------------------------------------------

-- Non-recursive datatypes referencing no other types:
-- * May use _apply1 in the matching instance
-- * May use _empty in the fall-through instance

instance Transform Bool Bool where
  bottomUp  = bottomUp_apply1
  topDown   = topDown_apply1
  bottomUpM = bottomUpM_apply1
  topDownM  = topDownM_apply1
  transform  = transform_apply1
  transformM = transformM_apply1
  transformA = transformA_apply1
instance Transform Bool a where
  bottomUp  = bottomUp_empty
  topDown   = topDown_empty
  bottomUpM = bottomUpM_empty
  topDownM  = topDownM_empty
  transform  = transform_empty
  transformM = transformM_empty
  transformA = transformA_empty

instance Transform () () where
  bottomUp  = bottomUp_apply1
  topDown   = topDown_apply1
  bottomUpM = bottomUpM_apply1
  topDownM  = topDownM_apply1
  transform  = transform_apply1
  transformM = transformM_apply1
  transformA = transformA_apply1
instance Transform () a where
  bottomUp  = bottomUp_empty
  topDown   = topDown_empty
  bottomUpM = bottomUpM_empty
  topDownM  = topDownM_empty
  transform  = transform_empty
  transformM = transformM_empty
  transformA = transformA_empty

--------------------------------------------------------------------------------

-- Non-recursive datatypes referencing other types:
-- * May use _apply1 in the matching instance
-- * Must use _default the fall-through instance

instance Transform (Maybe a) (Maybe a) where
  bottomUp  = bottomUp_apply1
  topDown   = topDown_apply1
  bottomUpM = bottomUpM_apply1
  topDownM  = topDownM_apply1
  transform  = transform_apply1
  transformM = transformM_apply1
  transformA = transformA_apply1
instance Transform b a => Transform (Maybe b) a

instance Transform (Either b c) (Either b c) where
  bottomUp  = bottomUp_apply1
  topDown   = topDown_apply1
  bottomUpM = bottomUpM_apply1
  topDownM  = topDownM_apply1
  transform  = transform_apply1
  transformM = transformM_apply1
  transformA = transformA_apply1
instance (Transform b a, Transform c a) => Transform (Either b c) a

instance Transform (b,c) (b,c) where
  bottomUp  = bottomUp_apply1
  topDown   = topDown_apply1
  bottomUpM = bottomUpM_apply1
  topDownM  = topDownM_apply1
  transform  = transform_apply1
  transformM = transformM_apply1
  transformA = transformA_apply1
instance (Transform b a, Transform c a) => Transform (b,c) a

instance Transform (b,c,d) (b,c,d) where
  bottomUp  = bottomUp_apply1
  topDown   = topDown_apply1
  bottomUpM = bottomUpM_apply1
  topDownM  = topDownM_apply1
  transform  = transform_apply1
  transformM = transformM_apply1
  transformA = transformA_apply1
instance (Transform b a, Transform c a, Transform d a) => Transform (b,c,d) a

instance Transform (b,c,d,e) (b,c,d,e) where
  bottomUp  = bottomUp_apply1
  topDown   = topDown_apply1
  bottomUpM = bottomUpM_apply1
  topDownM  = topDownM_apply1
  transform  = transform_apply1
  transformM = transformM_apply1
  transformA = transformA_apply1
instance (Transform b a, Transform c a, Transform d a, Transform e a) => Transform (b,c,d,e) a

instance Transform (b,c,d,e,f) (b,c,d,e,f) where
  bottomUp  = bottomUp_apply1
  topDown   = topDown_apply1
  bottomUpM = bottomUpM_apply1
  topDownM  = topDownM_apply1
  transform  = transform_apply1
  transformM = transformM_apply1
  transformA = transformA_apply1
instance (Transform b a, Transform c a, Transform d a, Transform e a, Transform f a) => Transform (b,c,d,e,f) a

instance Transform (b,c,d,e,f,g) (b,c,d,e,f,g) where
  bottomUp  = bottomUp_apply1
  topDown   = topDown_apply1
  bottomUpM = bottomUpM_apply1
  topDownM  = topDownM_apply1
  transform  = transform_apply1
  transformM = transformM_apply1
  transformA = transformA_apply1
instance (Transform b a, Transform c a, Transform d a, Transform e a, Transform f a, Transform g a) => Transform (b,c,d,e,f,g) a

instance Transform (b,c,d,e,f,g,h) (b,c,d,e,f,g,h) where
  bottomUp  = bottomUp_apply1
  topDown   = topDown_apply1
  bottomUpM = bottomUpM_apply1
  topDownM  = topDownM_apply1
  transform  = transform_apply1
  transformM = transformM_apply1
  transformA = transformA_apply1
instance (Transform b a, Transform c a, Transform d a, Transform e a, Transform f a, Transform g a, Transform h a) => Transform (b,c,d,e,f,g,h) a

--------------------------------------------------------------------------------

-- Recursive datatypes:
-- * Must use _apply in the matching instance
-- * Must use _default the fall-through instance

instance Transform a [a] => Transform [a] [a]
instance Transform b a => Transform [b] a

