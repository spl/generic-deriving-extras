{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}

--------------------------------------------------------------------------------

module Generics.Deriving.Zipper.Trace {-(
  Zipper(..),
  ZipperT,
  runZipperT,
  Dir(..),
  Path(..),
  -- *
  enter,
  leave,
  -- *
  up,
  down,
  downs,
  downl,
  move,
  moves,
  movel,
  -- *
  get,
  set,
  -- *
  Loc,
  Empty,
  (:<:),
  -- *
  module Data.Typeable,
)-} where

--------------------------------------------------------------------------------

import Data.Typeable
import Data.Dynamic

import Control.Arrow (second)
import Control.Applicative ((<$>), Applicative((<*>)), (<|>))
import Data.Maybe (fromJust)

import Control.Monad (liftM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Error
import Control.Monad.Trans.State.Lazy hiding (get)
import qualified Control.Monad.Trans.State.Lazy as State (get)

import Generics.Deriving.Base
import Generics.Deriving.Zipper.Base hiding (enter, leave, up, down, move, get, set)
import qualified Generics.Deriving.Zipper.Base as Base

--------------------------------------------------------------------------------

data Path' m r a b c d where
  End  ::                                                                                                             Path' m r a a Empty      Empty
  Down :: Dir -> String -> ((Zipper a, Zipper b, Monad m) => Loc a r c          -> ZipperT m (Loc b r (a :<: c)))  -> Path' m r a b c          (a :<: c)
  Move :: Dir -> String -> ((Zipper a, Zipper b, Monad m) => Loc a r (c :<: cs) -> ZipperT m (Loc b r (c :<: cs))) -> Path' m r a b (c :<: cs) (c :<: cs)

data Path where
  Path :: Path' m r a b c d -> Path

instance Error Path where
  noMsg = Path End

--------------------------------------------------------------------------------

newtype ZipperT m a = ZipperT { unZipperT :: StateT Path (ErrorT Path m) a }
  deriving (Functor, Applicative, Monad)

runZipperT :: Monad m => ZipperT m a -> m (Either Path a)
runZipperT z = liftM changeResults (runErrorT (runStateT (unZipperT z) (Path End)))
  where
    changeResults = either Left (Right . fst)

--------------------------------------------------------------------------------

z_log :: Monad m => Path -> ZipperT m ()
z_log (Path p) = ZipperT $ modify 

z_error :: Monad m => ZipperT m a
z_error = ZipperT $ State.get >>= lift . throwError

z_check :: Monad m => Maybe a -> ZipperT m a
z_check = maybe z_error return

{-

--------------------------------------------------------------------------------

up :: (Zipper a, Zipper b, Monad m) => Loc a r (b :<: c) -> ZipperT m (Loc b r c)
up = return . Base.up

downl :: (Zipper a, Zipper b, Monad m) => Dir -> proxy b -> String -> Loc a r c -> ZipperT m (Loc b r (a :<: c))
downl dir _ msg loc = do
  z_log (Down dir msg)
  z_check (Base.downo dir loc)

downs :: (Zipper a, Zipper b, Show (proxy b), Monad m) => Dir -> proxy b -> Loc a r c -> ZipperT m (Loc b r (a :<: c))
downs dir proxy loc = downl dir proxy (toDyn proxy) loc

down :: (Zipper a, Zipper b, Monad m) => Dir -> proxy b -> Loc a r c -> ZipperT m (Loc b r (a :<: c))
down dir proxy = downl dir proxy undefined

movel :: (Zipper a, Zipper b, Monad m) => Dir -> proxy b -> String -> Loc a r (c :<: cs) -> ZipperT m (Loc b r (c :<: cs))
movel dir _ msg loc = do
  z_log (Move dir msg)
  z_check (Base.moveo dir loc)

moves :: (Zipper a, Zipper b, Show (proxy b), Monad m) => Dir -> proxy b -> Loc a r (c :<: cs) -> ZipperT m (Loc b r (c :<: cs))
moves dir proxy loc = movel dir proxy (toDyn proxy) loc

move :: (Zipper a, Zipper b, Monad m) => Dir -> proxy b -> Loc a r (c :<: cs) -> ZipperT m (Loc b r (c :<: cs))
move dir proxy loc = movel dir proxy undefined loc

--------------------------------------------------------------------------------

enter :: (Zipper a, Monad m) => a -> ZipperT m (Loc a a Empty)
enter = return . Base.enter

leave :: (Zipper a, Monad m) => Loc a r c -> ZipperT m r
leave = return . Base.leave

--------------------------------------------------------------------------------

get :: Monad m => Loc a r c -> ZipperT m a
get = return . Base.get

set :: Monad m => a -> Loc a r c -> ZipperT m (Loc a r c)
set x = return . Base.set x

--------------------------------------------------------------------------------

trace loc@(Loc foc CNil) step = case step of
  Down L con -> downs L (fromDyn con) loc
  Down R con -> downs R (fromDyn con) loc
trace loc@(Loc foc (CCons {})) step = case step of
  Move L con -> moves L (fromDyn con) loc
  Move R con -> moves R (fromDyn con) loc
-- trace step loc = case step of

data PrimFam a where
  Char  ::                      PrimFam Char
  Int   ::                      PrimFam Int
  Float ::                      PrimFam Float
  List  :: Show (f a) => f a -> PrimFam [a]

deriving instance Show (PrimFam a)
deriving instance Typeable1 PrimFam

-}
