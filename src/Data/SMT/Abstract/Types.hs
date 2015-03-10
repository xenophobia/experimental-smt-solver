{-# Language GADTs #-}
{-# Language KindSignatures #-}
{-# LAnguage TypeOperators #-}
{-# Language ConstraintKinds #-}
{-# Language UndecidableInstances #-}
{-# Language ExistentialQuantification #-}
{-# Language Rank2Types #-}
{-# Language DataKinds #-}
{-# Language FlexibleInstances #-}
{-# Language FlexibleContexts #-}
{-# Language ScopedTypeVariables #-}
{-# Language MultiParamTypeClasses #-}
{-# Language UnicodeSyntax #-}

module Data.SMT.Abstract.Types where

import Data.Extensible
import qualified Data.IntSet as IS

data FComponent = EQUAL | LESSTHAN | AND | OR
data TComponent = VAR | INT | ADD | NEG | MUL

type AllFComponent = [EQUAL, LESSTHAN, AND, OR]
type AllTComponent = [VAR, INT, ADD, NEG, MUL]

class Ppr a where
  ppr :: a -> String

class GetVariables a where
  fv :: a -> IS.IntSet

data AbstFormula term (cs :: [FComponent]) (a :: FComponent) where
  (:=:) :: (EQUAL ∈ cs) => term -> term -> AbstFormula term cs EQUAL
  (:<:) :: (LESSTHAN ∈ cs) => term -> term -> AbstFormula term cs LESSTHAN
  (:&:) :: (AND ∈ cs) => AbstFormula term cs :| cs -> AbstFormula term cs :| cs -> AbstFormula term cs AND
  (:|:) :: (OR ∈ cs) => AbstFormula term cs :| cs -> AbstFormula term cs :| cs -> AbstFormula term cs OR

instance Ppr term => Ppr (AbstFormula term cs EQUAL) where
  ppr (t1 :=: t2) = ppr t1 ++ "=" ++ ppr t2
  
instance (Ppr term, Ppr (AbstFormula term cs :| cs)) => Ppr (AbstFormula term cs AND) where
  ppr (f1 :&: f2) = ppr f1 ++ "&" ++ ppr f2

instance Ppr term => Ppr (AbstFormula term cs LESSTHAN) where
  ppr (t1 :<: t2) = ppr t1 ++ "<" ++ ppr t2

instance (Ppr term, Ppr (AbstFormula term cs :| cs)) => Ppr (AbstFormula term cs OR) where
  ppr (f1 :|: f2) = ppr f1 ++ "|" ++ ppr f2

data AbstTerm (cs :: [TComponent]) (c :: TComponent) where
  Var :: (VAR ∈ cs) => Int -> AbstTerm cs VAR
  IConst :: (INT ∈ cs) => Int -> AbstTerm cs INT
  (:*:) :: (MUL ∈ cs) => AbstTerm cs :| cs -> AbstTerm cs :| cs -> AbstTerm cs MUL
  (:+:) :: (ADD ∈ cs) => AbstTerm cs :| cs -> AbstTerm cs :| cs -> AbstTerm cs ADD
  Neg :: (NEG ∈ cs) => AbstTerm cs :| cs -> AbstTerm cs NEG

instance Ppr (AbstTerm cs VAR) where
  ppr (Var i) = "X" ++ show i

instance Ppr (AbstTerm cs INT) where
  ppr (IConst i) = show i

instance Ppr (AbstTerm cs :| cs) => Ppr (AbstTerm cs ADD) where
  ppr (t1 :+: t2) = ppr t1 ++ "+" ++ ppr t2
  
instance Ppr (AbstTerm cs :| cs) => Ppr (AbstTerm cs MUL) where
  ppr (t1 :*: t2) = ppr t1 ++ "*" ++ ppr t2

instance Ppr (AbstTerm cs :| cs) => Ppr (AbstTerm cs NEG) where
  ppr (Neg t) = "(-" ++ ppr t ++ ")"

instance GetVariables (AbstTerm cs VAR) where
  fv (Var i) = IS.singleton i

instance GetVariables (AbstTerm cs INT) where
  fv _ = IS.empty

instance GetVariables (AbstTerm cs :| cs) => GetVariables (AbstTerm cs ADD) where
  fv (t1 :+: t2) = fv t1 `IS.union` fv t2
  
instance GetVariables (AbstTerm cs :| cs) => GetVariables (AbstTerm cs MUL) where
  fv (t1 :*: t2) = fv t1 `IS.union` fv t2

instance GetVariables (AbstTerm cs :| cs) => GetVariables (AbstTerm cs NEG) where
  fv (Neg t) = fv t

instance GetVariables term => GetVariables (AbstFormula term cs EQUAL) where
  fv (t1 :=: t2) = fv t1 `IS.union` fv t2

instance GetVariables term => GetVariables (AbstFormula term cs LESSTHAN) where
  fv (t1 :<: t2) = fv t1 `IS.union` fv t2

instance (GetVariables (AbstFormula term cs :| cs)) => GetVariables (AbstFormula term cs AND) where
  fv (f1 :&: f2) = fv f1 `IS.union` fv f2
  
instance (GetVariables (AbstFormula term cs :| cs)) => GetVariables (AbstFormula term cs OR) where
  fv (f1 :|: f2) = fv f1 `IS.union` fv f2