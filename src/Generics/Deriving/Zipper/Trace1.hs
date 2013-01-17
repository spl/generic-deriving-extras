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

module Generics.Deriving.Zipper.Trace (
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
) where

--------------------------------------------------------------------------------

import Data.Typeable
import Data.Data

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

data Step = Down Dir String | Move Dir String
  deriving Show

type Path = [Step]

type PathC = Path -> Path

instance Error (Path -> Path) where
  noMsg = id

--------------------------------------------------------------------------------

newtype ZipperT m a = ZipperT { unZipperT :: StateT PathC (ErrorT PathC m) a }
  deriving (Functor, Applicative, Monad)

runZipperT :: Monad m => ZipperT m a -> m (Either Path a)
runZipperT z = liftM changeResults (runErrorT (runStateT (unZipperT z) id))
  where
    changeResults = either (Left . ($ [])) (Right . fst)

--------------------------------------------------------------------------------

z_log :: Monad m => Step -> ZipperT m ()
z_log p = ZipperT $ modify (. (p:))

z_error :: Monad m => ZipperT m a
z_error = ZipperT $ State.get >>= lift . throwError

z_check :: Monad m => Maybe a -> ZipperT m a
z_check = maybe z_error return

--------------------------------------------------------------------------------

up :: (Zipper a, Zipper b, Monad m) => Loc a r (b :<: c) -> ZipperT m (Loc b r c)
up = return . Base.up

downl :: (Zipper a, Zipper b, Monad m) => Dir -> proxy b -> String -> Loc a r c -> ZipperT m (Loc b r (a :<: c))
downl dir _ msg loc = do
  z_log (Down dir msg)
  z_check (Base.downo dir loc)

downs :: (Zipper a, Zipper b, Show (proxy b), Monad m) => Dir -> proxy b -> Loc a r c -> ZipperT m (Loc b r (a :<: c))
downs dir proxy loc = downl dir proxy (show proxy) loc

down :: (Zipper a, Zipper b, Monad m) => Dir -> proxy b -> Loc a r c -> ZipperT m (Loc b r (a :<: c))
down dir proxy = downl dir proxy "<unknown>"

movel :: (Zipper a, Zipper b, Monad m) => Dir -> proxy b -> String -> Loc a r (c :<: cs) -> ZipperT m (Loc b r (c :<: cs))
movel dir _ msg loc = do
  z_log (Move dir msg)
  z_check (Base.moveo dir loc)

moves :: (Zipper a, Zipper b, Show (proxy b), Monad m) => Dir -> proxy b -> Loc a r (c :<: cs) -> ZipperT m (Loc b r (c :<: cs))
moves dir proxy loc = movel dir proxy (show proxy) loc

move :: (Zipper a, Zipper b, Monad m) => Dir -> proxy b -> Loc a r (c :<: cs) -> ZipperT m (Loc b r (c :<: cs))
move dir proxy loc = movel dir proxy "<unknown>" loc

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

{-
trace step proxy loc = case step
  Down L con -> downs L proxy
  Down R con ->
  Move L con ->
  Move R con ->
-}

data PrimFam a where
  Char  ::                      PrimFam Char
  Int   ::                      PrimFam Int
  Float ::                      PrimFam Float
  List  :: Show (f a) => f a -> PrimFam [a]

deriving instance Show (PrimFam a)

