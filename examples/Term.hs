{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}

-- Adapted from the instant-zipper package

module Term where

import Generics.Deriving.Zipper.Trace

-- | Datatype

data Term
  = Var    String
  | Lambda String Term
  | App    Term Term
  | If     Term Term Term
  deriving (Eq, Show, Generic, Typeable)

instance Zipper Term

-- | Proxy

data TermFam a where
  Term   :: TermFam Term
  String :: TermFam String

deriving instance Show (TermFam a)

-- | Example

fac :: Term
fac = Lambda "n"
  (If (App (App (Var "=") (Var "n")) (Var "0"))
      (Var "1")
      (App (App (Var "+") (Var "n"))
           (App (Var "fac")
                (App (Var "pred") (Var "n")))))


fixFac :: Term
fixFac = (\(Right x) -> x) $ runZipperM $
  enter fac
  >>= down L Term
  >>= down L Term
  >>= move R Term
  >>= move R Term
  >>= down L Term
  >>= down L Term
  >>= down L String
  >>= set "*"
  >>= leave

