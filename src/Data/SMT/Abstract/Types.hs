{-# Language GADTs #-}
{-# Language KindSignatures #-}
{-# LAnguage TypeOperators #-}
{-# Language DataKinds #-}
{-# Language FlexibleContexts #-}
{-# Language ScopedTypeVariables #-}
{-# Language UnicodeSyntax #-}

module Data.SMT.Abstract.Types where

import Data.Extensible.Sum
import Data.Extensible.Internal

data FComponent = EQUAL | LESSTHAN | AND | OR
data TComponent = VAR | INT | ADD

data AbstFormula term (cs :: [FComponent]) (a :: FComponent) where
  (:=:) :: (EQUAL ∈ cs) => term -> term -> AbstFormula term cs EQUAL
  (:<:) :: (LESSTHAN ∈ cs) => term -> term -> AbstFormula term cs LESSTHAN
  (:&:) :: (AND ∈ cs) => AbstFormula term cs :| cs -> AbstFormula term cs :| cs -> AbstFormula term cs AND
  (:|:) :: (OR ∈ cs) => AbstFormula term cs :| cs -> AbstFormula term cs :| cs -> AbstFormula term cs OR

-- lifted constructors
(<:=:>) :: (EQUAL ∈ cs) => term -> term -> AbstFormula term cs :| cs
t1 <:=:> t2 = embed (t1 :=: t2)

(<:<:>) :: (LESSTHAN ∈ cs) => term -> term -> AbstFormula term cs :| cs
t1 <:<:> t2 = embed (t1 :<: t2)

(<:&:>) :: (AND ∈ cs) => AbstFormula term cs :| cs -> AbstFormula term cs :| cs -> AbstFormula term cs :| cs
f1 <:&:> f2 = embed (f1 :&: f2)

(<:|:>) :: (OR ∈ cs) => AbstFormula term cs :| cs -> AbstFormula term cs :| cs -> AbstFormula term cs :| cs
f1 <:|:> f2 = embed (f1 :|: f2)

data AbstTerm (cs :: [TComponent]) (c :: TComponent) where
  Var :: (VAR ∈ cs) => Int -> AbstTerm cs VAR
  IConst :: (INT ∈ cs) => Int -> AbstTerm cs INT
  (:+:) :: (ADD ∈ cs) => AbstTerm cs :| cs -> AbstTerm cs :| cs -> AbstTerm cs ADD

-- lifted constructors
(<:+:>) :: (ADD ∈ cs) => AbstTerm cs :| cs -> AbstTerm cs :| cs -> AbstTerm cs :| cs
t1 <:+:> t2 = embed (t1 :+: t2)