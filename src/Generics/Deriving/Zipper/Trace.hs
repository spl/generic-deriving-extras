{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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
  downu,
  downl,
  move,
  moveu,
  movel,
  -- *
  get,
  set,
  modify,
  -- *
  Loc,
  Empty,
  (:<:),
  -- *
  Generic,
  Typeable,
) where

--------------------------------------------------------------------------------

import Generics.Deriving.Zipper.Base hiding (enter, leave, up, down, move, get, set, modify)
import qualified Generics.Deriving.Zipper.Base as Base

import Control.Applicative (Applicative)

import Control.Monad (liftM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Error
import Control.Monad.Trans.State.Lazy hiding (get, modify)
import qualified Control.Monad.Trans.State.Lazy as State (get, modify)

--------------------------------------------------------------------------------

data Step = Down Dir String | Move Dir String
  deriving Show

type Path = [Step]

type PathFun = Path -> Path

instance Error (Path -> Path) where
  noMsg = id

--------------------------------------------------------------------------------

newtype ZipperT m a = ZipperT { unZipperT :: StateT PathFun (ErrorT PathFun m) a }
  deriving (Functor, Applicative, Monad)

runZipperT :: Monad m => ZipperT m a -> m (Either Path a)
runZipperT z = liftM changeResults (runErrorT (runStateT (unZipperT z) id))
  where
    changeResults = either (Left . ($ [])) (Right . fst)

--------------------------------------------------------------------------------

z_log :: Monad m => Step -> ZipperT m ()
z_log p = ZipperT $ State.modify (. (:) p)

z_log_up :: Monad m => ZipperT m ()
z_log_up = ZipperT $ State.modify (tail .)

z_error :: Monad m => ZipperT m a
z_error = ZipperT $ State.get >>= lift . throwError

z_check :: Monad m => Maybe a -> ZipperT m a
z_check = maybe z_error return

--------------------------------------------------------------------------------

up :: (Zipper a, Zipper b, Monad m) => Loc a r (b :<: c) -> ZipperT m (Loc b r c)
up loc = do
  z_log_up
  return (Base.up loc)

downl :: (Zipper a, Zipper b, Monad m) => Dir -> proxy b -> String -> Loc a r c -> ZipperT m (Loc b r (a :<: c))
downl dir _ msg loc = do
  z_log (Down dir msg)
  z_check (Base.downo dir loc)

downu :: (Zipper a, Zipper b, Monad m) => Dir -> proxy b -> Loc a r c -> ZipperT m (Loc b r (a :<: c))
downu dir proxy = downl dir proxy "<unknown>"

down :: (Zipper a, Zipper b, Show (proxy b), Monad m) => Dir -> proxy b -> Loc a r c -> ZipperT m (Loc b r (a :<: c))
down dir proxy loc = downl dir proxy (show proxy) loc

movel :: (Zipper a, Zipper b, Monad m) => Dir -> proxy b -> String -> Loc a r (c :<: cs) -> ZipperT m (Loc b r (c :<: cs))
movel dir _ msg loc = do
  z_log (Move dir msg)
  z_check (Base.moveo dir loc)

moveu :: (Zipper a, Zipper b, Monad m) => Dir -> proxy b -> Loc a r (c :<: cs) -> ZipperT m (Loc b r (c :<: cs))
moveu dir proxy loc = movel dir proxy "<unknown>" loc

move :: (Zipper a, Zipper b, Show (proxy b), Monad m) => Dir -> proxy b -> Loc a r (c :<: cs) -> ZipperT m (Loc b r (c :<: cs))
move dir proxy loc = movel dir proxy (show proxy) loc

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

modify :: Monad m => (a -> a) -> Loc a r c -> ZipperT m (Loc a r c)
modify f = return . Base.modify f

