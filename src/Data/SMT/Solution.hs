module Data.SMT.Solution where

import Data.IntMap

data SMTSolution = Unsatisfied | Satisfied (IntMap Int)
