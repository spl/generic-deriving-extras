{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}

module Fold2Example where

import GHC.Generics
import Generics.Deriving.Fold2
import Data.Maybe (fromJust)

infix 1 :=

data Expr
  =  Const  Int
  |  Add    Expr  Expr
  |  Mul    Expr  Expr
  |  EVar   Var
  |  Let    Decl  Expr
  deriving (Show, Generic)

data Decl
  =  (:=)   Var    Expr
  |  Seq    [Decl]
  |  None
  deriving (Show, Generic)

type Var = String

data AST :: * -> * where
  Expr   ::  AST Expr
  Decl   ::  AST Decl
  DeclL  ::  AST [Decl]

type Env = [(Var, Int)]

data family Value a :: *
newtype instance Value Expr    =  EV  (Env -> Int)
newtype instance Value Decl    =  DV  (Env -> Env)
newtype instance Value [Decl]  =  DVL (Env -> Env)

infixr 5 &

(&) :: a -> b -> (a, b)
(&) = (,)

evalAlgebra :: Algebra AST Value
evalAlgebra Expr =
     (\ (Left x)                       -> EV (const x))
  &  (\ (Right (EV x)) (Right (EV y))  -> EV (\ env -> x env  +  y env))
  &  (\ (Right (EV x)) (Right (EV y))  -> EV (\ env -> x env  *  y env))
  &  (\ (Left x)                       -> EV (fromJust . lookup x))
  &  (\ (Right (DV e)) (Right (EV x))  -> EV (\ env -> x (e env)))
evalAlgebra Decl =
     (\ (Left x) (Right (EV v))        -> DV (\ env -> (x, v env) : env ))
  &  (\ (Right (DVL f))                -> DV f)
  &  (                                    DV id)
evalAlgebra DeclL =
     (                                    DVL id)
  &  (\ (Right (DV f)) (Right (DVL g)) -> DVL (f . g))

{-
evalAlgebra :: Algebra AST Value
evalAlgebra _ =
     (  (\ x             -> EV (const x))
     &  (\ (EV x) (EV y) -> EV (\ env -> x env  +  y env))
     &  (\ (EV x) (EV y) -> EV (\ env -> x env  *  y env))
     &  (\ (VV x)        -> EV (fromJust . lookup x))
     &  (\ (DV e) (EV x) -> EV (\ env -> x (e env)))
     )
  &  (  (\ (VV x) (EV v) -> DV (\ env -> (x, v env) : env ))
     &  (\ fs            -> DV (foldl (\ f (DV g) -> f . g) id fs))
     &  (                   DV id)
     )
  &     (\ x             -> VV x)
-}

instance Fold AST Expr   where proxy = Just Expr
instance Fold AST Decl   where proxy = Just Decl
instance Fold AST [Decl] where proxy = Just DeclL
instance Fold AST String
instance Fold AST Char
instance Fold AST Int

eval :: Expr -> Env -> Int
eval x = let EV f = fold evalAlgebra Expr x in f

example :: Expr
example = Let (Seq ["x" := Mul (Const 6) (Const 9), "z" := Const 1])
              (Mul (EVar "z") (Add (EVar "x") (EVar "y")))

test :: Int
test = eval example [("y", -12)]

