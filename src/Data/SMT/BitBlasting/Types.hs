{-# Language DataKinds #-}
{-# Language KindSignatures #-}
{-# Language TypeOperators #-}
{-# Language TypeSynonymInstances #-}
{-# Language FlexibleInstances #-}
module Data.SMT.BitBlasting.Types where

import qualified Data.IntSet as IS
import Data.Extensible.Sum
import Data.SMT.Abstract.Types

type TermComponents = [VAR, INT, ADD, NEG]
type FormulaComponents = [EQUAL, AND, LESSTHAN]

type TermOf = AbstTerm TermComponents
type Term = TermOf :| TermComponents
type FormulaOf = AbstFormula Term FormulaComponents
type Formula = FormulaOf :| FormulaComponents

instance GetVariables Term where
  fv = (fv :: TermOf VAR -> IS.IntSet)
       <:| (fv :: TermOf INT -> IS.IntSet)
       <:| (fv :: TermOf ADD -> IS.IntSet)
       <:| (fv :: TermOf NEG -> IS.IntSet)
       <:| exhaust
  
instance GetVariables Formula where
  fv = (fv :: FormulaOf EQUAL -> IS.IntSet)
       <:| (fv :: FormulaOf AND -> IS.IntSet)
       <:| (fv :: FormulaOf LESSTHAN -> IS.IntSet)
       <:| exhaust
