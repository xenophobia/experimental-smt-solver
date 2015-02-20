{-# Language KindSignatures #-}
{-# LAnguage TypeOperators #-}
{-# Language DataKinds #-}
{-# Language GADTs #-}
{-# Language TypeSynonymInstances #-}
{-# Language FlexibleInstances #-}
{-# Language ScopedTypeVariables #-}

module Data.SMT.BitBlasting.Types where

import Data.Extensible.Sum
import Data.SMT.Abstract.Types

type TermComponent a = AbstTerm [VAR, INT, ADD] a
type FormulaComponent a = AbstFormula Term [EQUAL, AND] a

type Term = AbstTerm [VAR, INT, ADD] :| [VAR, INT, ADD]
type Formula = AbstFormula Term [EQUAL, AND] :| [EQUAL, AND]

instance Show Formula where
  show =   (\(t1 :=: t2) -> show t1 ++ "=" ++ show t2)
       <:| (\(f1 :&: f2) -> show f1 ++ "&" ++ show f2)
       <:| exhaust

instance Show Term where
  show =   (\(Var i) -> "X" ++ show i)
       <:| (\(IConst i) -> show i)
       <:| (\(t1 :+: t2) -> show t1 ++ "+" ++ show t2)
       <:| exhaust