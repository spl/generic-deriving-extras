{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}

-- Adapted from the instant-zipper package

module Dept where

import Generics.Deriving.Zipper.Trace

-- | Datatypes

type Salary  = Float
type Manager = Employee
type Name    = String

data Dept = D Manager [Employee]
  deriving (Eq, Show, Generic, Typeable)

data Employee = E Name Salary
  deriving (Eq, Show, Generic, Typeable)

instance Zipper Dept
instance Zipper Employee

-- | Proxies

data DeptFam a where
  Dept     :: DeptFam Dept
  Employee :: DeptFam Employee
  Salary   :: DeptFam Salary
  Name     :: DeptFam Name

deriving instance Show (DeptFam a)

data PrimFam a where
  Char  ::                      PrimFam Char
  Int   ::                      PrimFam Int
  Float ::                      PrimFam Float
  List  :: Show (f a) => f a -> PrimFam [a]

deriving instance Show (PrimFam a)

-- | Example

dept :: Dept
dept = D doaitse [johan, sean, pedro]
  where
    doaitse = E "Doaitse" 8000
    johan   = E "Johan"   8000
    sean    = E "Sean"    2600
    pedro   = E "Pedro"   2400

fixDept :: Dept
fixDept = (\(Right x) -> x) $ runZipperM $
  enter dept
  >>= down L Employee
  >>= down L Name
  >>= set "prof. dr. Swierstra"
  >>= move R Salary
  >>= modify (+1000)
  >>= up
  >>= up
  >>= down R (List Employee)
  >>= down L (List Employee)
  >>= down L Employee
  >>= down R Salary
  >>= modify (subtract 1600)
  >>= leave

