module Data.SMT.BitBlasting.Solver where

import Data.Bits
import Data.IntMap (IntMap, union, singleton)
import qualified Data.IntMap as IM
import Data.SMT.Types
import Data.SMT.Solution

import Control.Monad.State

import Ersatz hiding (Var)

bitblasting :: Int -> SMT -> IO (Maybe SMTSolution)
bitblasting width smt = do
  (result, ~(Just answer)) <- minisat `solveWith` flattened
  case result of
    Unsolved -> return Nothing
    Ersatz.Unsatisfied -> return $ Just (Data.SMT.Solution.Unsatisfied)
    Ersatz.Satisfied -> return $ Just (Data.SMT.Solution.Satisfied (IM.map (foldl (\acc b -> acc*2+(if b then 1 else 0)) 0) answer))
    where flattened = flattening width smt

flattening :: (MonadState s m, HasSAT s) => Int -> SMT -> m (IntMap [Bit])
flattening width (t1 :=: t2) = do
  (t1Flattened, m1) <- flatteningTerm width t1
  (t2Flattened, m2) <- flatteningTerm width t2
  assert $ t1Flattened === t2Flattened
  return $ m1 `union` m2

flatteningTerm :: (MonadState s m, HasSAT s) => Int -> Term -> m ([Bit], IntMap [Bit])
flatteningTerm width (Const n) = return $ ([if testBit n i then true else false | i <- [width-1, width-2 .. 0]], IM.empty)
flatteningTerm width (Var n) = do
  bs <- replicateM width exists
  return $ (bs, singleton n bs)
flatteningTerm _ _ = undefined
