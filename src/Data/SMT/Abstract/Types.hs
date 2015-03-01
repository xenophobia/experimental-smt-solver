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
data TComponent = VAR | INT | ADD | NEG

data AbstFormula term (cs :: [FComponent]) (a :: FComponent) where
  (:=:) :: (EQUAL ∈ cs) => term -> term -> AbstFormula term cs EQUAL
  (:<:) :: (LESSTHAN ∈ cs) => term -> term -> AbstFormula term cs LESSTHAN
  (:&:) :: (AND ∈ cs) => AbstFormula term cs :| cs -> AbstFormula term cs :| cs -> AbstFormula term cs AND
  (:|:) :: (OR ∈ cs) => AbstFormula term cs :| cs -> AbstFormula term cs :| cs -> AbstFormula term cs OR

instance Show term => Show (AbstFormula term cs EQUAL) where
  show (t1 :=: t2) = show t1 ++ "=" ++ show t2
  
instance (Show term, Show (AbstFormula term cs :| cs)) => Show (AbstFormula term cs AND) where
  show (f1 :&: f2) = show f1 ++ "&" ++ show f2

instance Show term => Show (AbstFormula term cs LESSTHAN) where
  show (t1 :<: t2) = show t1 ++ "<" ++ show t2

instance (Show term, Show (AbstFormula term cs :| cs)) => Show (AbstFormula term cs OR) where
  show (f1 :|: f2) = show f1 ++ "|" ++ show f2

data AbstTerm (cs :: [TComponent]) (c :: TComponent) where
  Var :: (VAR ∈ cs) => Int -> AbstTerm cs VAR
  IConst :: (INT ∈ cs) => Int -> AbstTerm cs INT
  (:+:) :: (ADD ∈ cs) => AbstTerm cs :| cs -> AbstTerm cs :| cs -> AbstTerm cs ADD
  Neg :: (NEG ∈ cs) => AbstTerm cs :| cs -> AbstTerm cs NEG

instance Show (AbstTerm cs VAR) where
  show (Var i) = "X" ++ show i

instance Show (AbstTerm cs INT) where
  show (IConst i) = show i

instance Show (AbstTerm cs :| cs) => Show (AbstTerm cs ADD) where
  show (t1 :+: t2) = show t1 ++ "+" ++ show t2

instance Show (AbstTerm cs :| cs) => Show (AbstTerm cs NEG) where
  show (Neg t) = "(-" ++ show t ++ ")"

class GetVariables a where
  fv :: a -> IS.IntSet

instance GetVariables (AbstTerm cs VAR) where
  fv (Var i) = IS.singleton i

instance GetVariables (AbstTerm cs INT) where
  fv _ = IS.empty

instance GetVariables (AbstTerm cs :| cs) => GetVariables (AbstTerm cs ADD) where
  fv (t1 :+: t2) = fv t1 `IS.union` fv t2

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


