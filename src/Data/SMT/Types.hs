module Data.SMT.Types where

data SMT = Term :=: Term deriving (Eq, Show)
data Term = Var Int | Const Int | Term :+: Term deriving (Eq, Show)
