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
  -- *
  Transform(..),
  -- *
  -- **
  bottomUp_apply,
  topDown_apply,
  bottomUpM_apply,
  topDownM_apply,
  -- **
  bottomUp_apply1,
  topDown_apply1,
  bottomUpM_apply1,
  topDownM_apply1,
  -- *
  -- **
  bottomUp_default,
  topDown_default,
  bottomUpM_default,
  topDownM_default,
  -- **
  bottomUp_empty,
  topDown_empty,
  bottomUpM_empty,
  topDownM_empty,
) where

--------------------------------------------------------------------------------

import GHC.Generics
import Control.Monad (liftM, liftM2, (<=<))

--------------------------------------------------------------------------------

type T  b a =                       (a ->   a) -> b ->   b
type TM b a = forall m . Monad m => (a -> m a) -> b -> m b

--------------------------------------------------------------------------------

class Transform' f a where
  bottomUp'  :: T  (f x) a
  topDown'   :: T  (f x) a
  bottomUpM' :: TM (f x) a
  topDownM'  :: TM (f x) a

instance Transform' U1 a where
  bottomUp'  _ U1 = U1
  topDown'   _ U1 = U1
  bottomUpM' _ U1 = return U1
  topDownM'  _ U1 = return U1

instance Transform b a => Transform' (K1 i b) a where
  bottomUp'  f (K1 x) = K1 (bottomUp f x)
  topDown'   f (K1 x) = K1 (topDown  f x)
  bottomUpM' f (K1 x) = liftM K1 (bottomUpM f x)
  topDownM'  f (K1 x) = liftM K1 (topDownM  f x)

instance Transform' f a => Transform' (M1 i c f) a where
  bottomUp'  f (M1 x) = M1 (bottomUp' f x)
  topDown'   f (M1 x) = M1 (topDown'  f x)
  bottomUpM' f (M1 x) = liftM M1 (bottomUpM' f x)
  topDownM'  f (M1 x) = liftM M1 (topDownM'  f x)

instance (Transform' f a, Transform' g a) => Transform' (f :+: g) a where
  bottomUp'  f (L1 x) = L1 (bottomUp' f x)
  bottomUp'  f (R1 x) = R1 (bottomUp' f x)
  topDown'   f (L1 x) = L1 (topDown'  f x)
  topDown'   f (R1 x) = R1 (topDown'  f x)
  bottomUpM' f (L1 x) = liftM L1 (bottomUpM' f x)
  bottomUpM' f (R1 x) = liftM R1 (bottomUpM' f x)
  topDownM'  f (L1 x) = liftM L1 (topDownM'  f x)
  topDownM'  f (R1 x) = liftM R1 (topDownM'  f x)

instance (Transform' f a, Transform' g a) => Transform' (f :*: g) a where
  bottomUp'  f (x :*: y) = bottomUp' f x :*: bottomUp' f y
  topDown'   f (x :*: y) = topDown'  f x :*: topDown'  f y
  bottomUpM' f (x :*: y) = liftM2 (:*:) (bottomUpM' f x) (bottomUpM' f y)
  topDownM'  f (x :*: y) = liftM2 (:*:) (topDownM'  f x) (topDownM'  f y)

--------------------------------------------------------------------------------

bottomUp_apply, topDown_apply :: (Generic a, Transform' (Rep a) a) => T a a
bottomUp_apply f = f . bottomUp_default f
topDown_apply  f = topDown_default f . f

bottomUpM_apply, topDownM_apply :: (Generic a, Transform' (Rep a) a) => TM a a
bottomUpM_apply f = f <=< bottomUpM_default f
topDownM_apply  f = topDownM_default f <=< f

--------------------------------------------------------------------------------

bottomUp_apply1, topDown_apply1 :: T a a
bottomUp_apply1 = id
topDown_apply1  = id

bottomUpM_apply1, topDownM_apply1 :: TM a a
bottomUpM_apply1 = id
topDownM_apply1  = id

--------------------------------------------------------------------------------

bottomUp_default, topDown_default :: (Generic b, Transform' (Rep b) a) => T b a
bottomUp_default f = to . bottomUp' f . from
topDown_default  f = to . topDown'  f . from

bottomUpM_default, topDownM_default :: (Generic b, Transform' (Rep b) a) => TM b a
bottomUpM_default f = liftM to . bottomUpM' f . from
topDownM_default  f = liftM to . topDownM'  f . from

--------------------------------------------------------------------------------

bottomUp_empty, topDown_empty :: T b a
bottomUp_empty _ = id
topDown_empty  _ = id

bottomUpM_empty, topDownM_empty :: TM b a
bottomUpM_empty _ = return
topDownM_empty  _ = return

--------------------------------------------------------------------------------

class TransformAlt b a where
  bottomUpAlt  :: T  b a
  topDownAlt   :: T  b a
  bottomUpMAlt :: TM b a
  topDownMAlt  :: TM b a

instance (Generic a, Transform' (Rep a) a) => TransformAlt a a where
  bottomUpAlt  = bottomUp_apply
  topDownAlt   = topDown_apply
  bottomUpMAlt = bottomUpM_apply
  topDownMAlt  = topDownM_apply

instance (Generic b, Transform' (Rep b) a) => TransformAlt b a where
  bottomUpAlt  = bottomUp_default
  topDownAlt   = topDown_default
  bottomUpMAlt = bottomUpM_default
  topDownMAlt  = topDownM_default

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

--------------------------------------------------------------------------------

-- Primitive types:
-- * Must use _apply1 in the matching instance
-- * May use _empty in the fall-through instance

instance Transform Char Char where
  bottomUp  = bottomUp_apply1
  topDown   = topDown_apply1
  bottomUpM = bottomUpM_apply1
  topDownM  = topDownM_apply1
instance Transform Char a where
  bottomUp  = bottomUp_empty
  topDown   = topDown_empty
  bottomUpM = bottomUpM_empty
  topDownM  = topDownM_empty

instance Transform Float Float where
  bottomUp  = bottomUp_apply1
  topDown   = topDown_apply1
  bottomUpM = bottomUpM_apply1
  topDownM  = topDownM_apply1
instance Transform Float a where
  bottomUp  = bottomUp_empty
  topDown   = topDown_empty
  bottomUpM = bottomUpM_empty
  topDownM  = topDownM_empty

instance Transform Double Double where
  bottomUp  = bottomUp_apply1
  topDown   = topDown_apply1
  bottomUpM = bottomUpM_apply1
  topDownM  = topDownM_apply1
instance Transform Double a where
  bottomUp  = bottomUp_empty
  topDown   = topDown_empty
  bottomUpM = bottomUpM_empty
  topDownM  = topDownM_empty

instance Transform Int Int where
  bottomUp  = bottomUp_apply1
  topDown   = topDown_apply1
  bottomUpM = bottomUpM_apply1
  topDownM  = topDownM_apply1
instance Transform Int a where
  bottomUp  = bottomUp_empty
  topDown   = topDown_empty
  bottomUpM = bottomUpM_empty
  topDownM  = topDownM_empty

--------------------------------------------------------------------------------

-- Non-recursive datatypes referencing no other types:
-- * May use _apply1 in the matching instance
-- * May use _empty in the fall-through instance

instance Transform Bool Bool where
  bottomUp  = bottomUp_apply1
  topDown   = topDown_apply1
  bottomUpM = bottomUpM_apply1
  topDownM  = topDownM_apply1
instance Transform Bool a where
  bottomUp  = bottomUp_empty
  topDown   = topDown_empty
  bottomUpM = bottomUpM_empty
  topDownM  = topDownM_empty

instance Transform () () where
  bottomUp  = bottomUp_apply1
  topDown   = topDown_apply1
  bottomUpM = bottomUpM_apply1
  topDownM  = topDownM_apply1
instance Transform () a where
  bottomUp  = bottomUp_empty
  topDown   = topDown_empty
  bottomUpM = bottomUpM_empty
  topDownM  = topDownM_empty

--------------------------------------------------------------------------------

-- Non-recursive datatypes referencing other types:
-- * May use _apply1 in the matching instance
-- * Must use _default the fall-through instance

instance Transform (Maybe a) (Maybe a) where
  bottomUp  = bottomUp_apply1
  topDown   = topDown_apply1
  bottomUpM = bottomUpM_apply1
  topDownM  = topDownM_apply1
instance Transform b a => Transform (Maybe b) a

instance Transform (Either b c) (Either b c) where
  bottomUp  = bottomUp_apply1
  topDown   = topDown_apply1
  bottomUpM = bottomUpM_apply1
  topDownM  = topDownM_apply1
instance (Transform b a, Transform c a) => Transform (Either b c) a

instance Transform (b,c) (b,c) where
  bottomUp  = bottomUp_apply1
  topDown   = topDown_apply1
  bottomUpM = bottomUpM_apply1
  topDownM  = topDownM_apply1
instance (Transform b a, Transform c a) => Transform (b,c) a

instance Transform (b,c,d) (b,c,d) where
  bottomUp  = bottomUp_apply1
  topDown   = topDown_apply1
  bottomUpM = bottomUpM_apply1
  topDownM  = topDownM_apply1
instance (Transform b a, Transform c a, Transform d a) => Transform (b,c,d) a

instance Transform (b,c,d,e) (b,c,d,e) where
  bottomUp  = bottomUp_apply1
  topDown   = topDown_apply1
  bottomUpM = bottomUpM_apply1
  topDownM  = topDownM_apply1
instance (Transform b a, Transform c a, Transform d a, Transform e a) => Transform (b,c,d,e) a

instance Transform (b,c,d,e,f) (b,c,d,e,f) where
  bottomUp  = bottomUp_apply1
  topDown   = topDown_apply1
  bottomUpM = bottomUpM_apply1
  topDownM  = topDownM_apply1
instance (Transform b a, Transform c a, Transform d a, Transform e a, Transform f a) => Transform (b,c,d,e,f) a

instance Transform (b,c,d,e,f,g) (b,c,d,e,f,g) where
  bottomUp  = bottomUp_apply1
  topDown   = topDown_apply1
  bottomUpM = bottomUpM_apply1
  topDownM  = topDownM_apply1
instance (Transform b a, Transform c a, Transform d a, Transform e a, Transform f a, Transform g a) => Transform (b,c,d,e,f,g) a

instance Transform (b,c,d,e,f,g,h) (b,c,d,e,f,g,h) where
  bottomUp  = bottomUp_apply1
  topDown   = topDown_apply1
  bottomUpM = bottomUpM_apply1
  topDownM  = topDownM_apply1
instance (Transform b a, Transform c a, Transform d a, Transform e a, Transform f a, Transform g a, Transform h a) => Transform (b,c,d,e,f,g,h) a

--------------------------------------------------------------------------------

-- Recursive datatypes:
-- * Must use _apply in the matching instance
-- * Must use _default the fall-through instance

instance Transform a [a] => Transform [a] [a]
instance Transform b a => Transform [b] a

