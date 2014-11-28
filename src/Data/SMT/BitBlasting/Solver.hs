{-# Language ViewPatterns #-}
module Data.SMT.BitBlasting.Solver where

import Prelude hiding ((&&), (||))
import Data.Bits
import Data.List (intersect)
import Data.IntMap (IntMap, union, singleton)
import qualified Data.IntMap as IM
import Data.SMT.Types
import Data.SMT.Solution

import Control.Monad
import Control.Monad.State

import Ersatz hiding (Var)

instance Equatable a => Equatable (IntMap a) where
  m1 === m2 = foldl (&&) true $ map (\k -> m1 IM.! k === m2 IM.! k) overlap
    where overlap = IM.keys m1 `intersect` IM.keys m2

decodeToTwoComplement :: Int -> [Bool] -> Int
decodeToTwoComplement _ [] = undefined
decodeToTwoComplement width (map (\x -> if x then 1 else 0) -> b:bs) = 
  foldl (\acc b -> acc*2+b) 0 (-b:bs)

bitblasting :: Int -> SMT -> IO (Maybe SMTSolution)
bitblasting width smt = do
  (result, ~(Just answer)) <- minisat `solveWith` flattened
  case result of
    Unsolved -> return Nothing
    Ersatz.Unsatisfied -> return $ Just (Data.SMT.Solution.Unsatisfied)
    Ersatz.Satisfied -> return $ Just (Data.SMT.Solution.Satisfied (IM.map (decodeToTwoComplement width) answer))
    where flattened = flattening width smt

flattening :: (MonadState s m, HasSAT s) => Int -> SMT -> m (IntMap [Bit])
flattening width (t1 :=: t2) = do
  (t1Flattened, m1) <- flatteningTerm width t1
  (t2Flattened, m2) <- flatteningTerm width t2
  assert $ t1Flattened === t2Flattened
  assert $ m1 === m2
  return $ m1 `union` m2
flattening width (smt1 :&: smt2) = do
  m1 <- flattening width smt1
  m2 <- flattening width smt2
  assert $ m1 === m2
  return $ m1 `union` m2

flatteningTerm :: (MonadState s m, HasSAT s) => Int -> Term -> m ([Bit], IntMap [Bit])
flatteningTerm width (Const n) | -(2^(width-1)) <= n && n < 2^(width-1) = return $ ([if testBit n i then true else false | i <- [width-1, width-2 .. 0]], IM.empty)
                               | otherwise = assert false >> return (replicate width false, IM.empty)                             
flatteningTerm width (Var n) = do
  bs <- replicateM width exists
  return $ (bs, singleton n bs)
flatteningTerm width (t1 :+: t2) = do
  (t1Flattened, m1) <- flatteningTerm width t1
  (t2Flattened, m2) <- flatteningTerm width t2
  flattened <- adder width t1Flattened t2Flattened
  assert $ (head t1Flattened /== head t2Flattened)
           || (head t1Flattened === head flattened) -- overflow detection
  return (flattened, m1 `union` m2)
flatteningTerm _ _ = undefined

adder :: (MonadState s m, HasSAT s) => Int -> [Bit] -> [Bit] -> m [Bit]
adder width bs1 bs2 = do
  bs <- replicateM width exists
  foldM_ aux false $ (reverse $ zip3 bs1 bs2 bs)
  return bs
  where
    aux cin (b1, b2, b) = do
      let (b', cout) = full_adder b1 b2 cin
      assert $ b === b'
      return cout
  