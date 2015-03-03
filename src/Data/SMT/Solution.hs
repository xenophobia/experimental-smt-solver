module Data.SMT.Solution where

import Data.IntMap

data Solution = Unknown | Unsatisfied | Satisfied (IntMap Int)
