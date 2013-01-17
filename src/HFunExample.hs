{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}

module Fold2Example where

import GHC.Generics
import Generics.Deriving.HFunctor

data T
  =  TC Int Char
  |  T  T
  deriving (Show, Generic)

data U
  =  UC Float Char
  |  U  U
  deriving (Show, Generic)

data Proxy :: * -> * -> * where
  Int_Float :: Proxy Int  Float
  Char_Char :: Proxy Char Char
  T_U       :: Proxy T    U

instance HFunctor Proxy Char Char  where proxy = Char_Char
instance HFunctor Proxy Int Float  where proxy = Int_Float
instance HFunctor Proxy T   U      where proxy = T_U

test = hmap T_U

