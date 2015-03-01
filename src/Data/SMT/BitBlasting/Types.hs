{-# Language DataKinds #-}
{-# Language KindSignatures #-}
{-# Language TypeOperators #-}
{-# Language TypeSynonymInstances #-}
{-# Language FlexibleInstances #-}
module Data.SMT.BitBlasting.Types where

import Data.Extensible.Sum
import Data.SMT.Abstract.Types

type TermComponents = [VAR, INT, ADD, NEG]
type FormulaComponents = [EQUAL, AND]

type TermOf = AbstTerm TermComponents
type Term = TermOf :| TermComponents
type FormulaOf = AbstFormula Term FormulaComponents
type Formula = FormulaOf :| FormulaComponents
