module Data.SMT.Types where

data AbstFormula term = term :=: term
                      | term :<: term
                      | (AbstFormula term) :&: (AbstFormula term)
                      | (AbstFormula term) :|: (AbstFormula term) deriving (Eq, Show)

data Term = Var Int | Const Int | Term :+: Term deriving (Eq, Show)
