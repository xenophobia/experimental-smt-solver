{-# Language DataKinds #-}
{-# Language KindSignatures #-}
{-# Language TypeOperators #-}
{-# Language TypeSynonymInstances #-}
{-# Language FlexibleInstances #-}
module Data.SMT.BitBlasting.Types where

import qualified Data.IntSet as IS
import Data.Extensible.Sum
import Data.SMT.Abstract.Types

type TermComponents = [VAR, INT, ADD, MUL, NEG]
type FormulaComponents = [EQUAL, AND, LESSTHAN]

type TermOf = AbstTerm TermComponents
type Term = TermOf :| TermComponents
type FormulaOf = AbstFormula Term FormulaComponents
type Formula = FormulaOf :| FormulaComponents

instance Ppr Term where
  ppr = (ppr :: TermOf VAR -> String)
        <:| (ppr :: TermOf INT -> String)
        <:| (ppr :: TermOf ADD -> String)
        <:| (ppr :: TermOf MUL -> String)
        <:| (ppr :: TermOf NEG -> String)
        <:| exhaust

instance Ppr Formula where
  ppr = (ppr :: FormulaOf EQUAL -> String)
        <:| (ppr :: FormulaOf AND -> String)
        <:| (ppr :: FormulaOf LESSTHAN -> String)
        <:| exhaust

instance GetVariables Term where
  fv = (fv :: TermOf VAR -> IS.IntSet)
       <:| (fv :: TermOf INT -> IS.IntSet)
       <:| (fv :: TermOf ADD -> IS.IntSet)
       <:| (fv :: TermOf MUL -> IS.IntSet)
       <:| (fv :: TermOf NEG -> IS.IntSet)
       <:| exhaust
  
instance GetVariables Formula where
  fv = (fv :: FormulaOf EQUAL -> IS.IntSet)
       <:| (fv :: FormulaOf AND -> IS.IntSet)
       <:| (fv :: FormulaOf LESSTHAN -> IS.IntSet)
       <:| exhaust

-- configuration

data Config = Config {
  startWidth :: Int
  , maxWidth :: Int 
  }

defaultConfig :: Config
defaultConfig = Config 2 10
