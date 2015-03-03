{-# Language ViewPatterns #-}
{-# Language GADTs #-}
{-# LAnguage TypeOperators #-}
{-# Language DataKinds #-}
{-# Language ScopedTypeVariables #-}

module Data.SMT.BitBlasting.Solver where

import Debug.Trace
import Prelude hiding ((&&), (||), not)
import Data.Bits
import Data.List (intersect)
import Data.IntMap (IntMap, union, singleton)
import qualified Data.IntMap as IM
import Data.SMT.BitBlasting.Types
import Data.SMT.Abstract.Types
import Data.SMT.Solution
import Data.Extensible.Sum

import Control.Monad
import Control.Monad.State
import qualified Data.IntSet as IS

import Ersatz hiding (Var, Satisfied, Unsatisfied, Unsolved, Solution)
import qualified Ersatz

instance Equatable a => Equatable (IntMap a) where
  m1 === m2 = foldl (&&) true $ map (\k -> m1 IM.! k === m2 IM.! k) overlap
    where overlap = IM.keys m1 `intersect` IM.keys m2

decodeToTwoComplement :: [Bool] -> Int
decodeToTwoComplement [] = undefined
decodeToTwoComplement (map (\x -> if x then 1 else 0) -> b:bs) = 
  foldl (\acc n -> acc*2+n) 0 (-b:bs)

bitblasting :: Int -> Formula -> IO Solution
bitblasting width _fm = do
  (result, ~(Just answer)) <- minisat `solveWith` flattened
  case result of
    Ersatz.Unsolved -> return Unknown
    Ersatz.Unsatisfied -> return Unsatisfied
    Ersatz.Satisfied -> return $ Satisfied (IM.map decodeToTwoComplement answer)
    where
      fm = foldl (\acc i -> embed (acc :&: embed (embed (Var i) :=: embed (Var i)))) _fm $ IS.toList (fv _fm)
      flattened = flattening width fm

flattening :: (MonadState s m, HasSAT s) => Int -> Formula -> m (IntMap [Bit])
flattening width = flatteningEQUAL <:| flatteningAND <:| flatteningLESSTHAN <:| exhaust
  where
    flatteningEQUAL (t1 :=: t2 :: FormulaOf EQUAL) = do
      (t1Flattened, m1) <- flatteningTerm width t1
      (t2Flattened, m2) <- flatteningTerm width t2
      assert $ t1Flattened === t2Flattened
      assert $ m1 === m2
      return $ m1 `union` m2
    flatteningAND (f1 :&: f2 :: FormulaOf AND) = do
      m1 <- flattening width f1
      m2 <- flattening width f2
      assert $ m1 === m2
      return $ m1 `union` m2
    flatteningLESSTHAN (t1 :<: t2 :: FormulaOf LESSTHAN) = do
      (t1Flattened, m1) <- flatteningTerm width t1
      (t2Flattened, m2) <- flatteningTerm width t2
      t1Flattened `isLessThan` t2Flattened
      assert $ m1 === m2
      return $ m1 `union` m2

isLessThan :: (MonadState s m, HasSAT s) => [Bit] -> [Bit] -> m ()
isLessThan (b1:bs1) (b2:bs2) = assert $ (foldr (\(x, y) acc -> (not x && y) || (x === y && acc)) false (zip (not b1 : bs1) (not b2 : bs2)))
isLessThan _ _ = error "isLessThan"

flatteningTerm :: (MonadState s m, HasSAT s) => Int -> Term -> m ([Bit], IntMap [Bit])
flatteningTerm width = flatteningTermVAR
                       <:| flatteningTermINT
                       <:| flatteningTermADD
                       <:| flatteningTermMUL
                       <:| flatteningTermNeg <:| exhaust
  where
    flatteningTermVAR (Var n :: TermOf VAR) = do
      bs <- replicateM width exists
      return $ (bs, singleton n bs)
    flatteningTermINT (IConst n :: TermOf INT)
      | -(2^(width-1)) <= n && n < 2^(width-1) = return $ ([if testBit n i then true else false | i <- [width-1, width-2 .. 0]], IM.empty)
      | otherwise = assert false >> return (replicate width false, IM.empty)
    flatteningTermADD (t1 :+: t2 :: TermOf ADD) = do
      (t1Flattened, m1) <- flatteningTerm width t1
      (t2Flattened, m2) <- flatteningTerm width t2
      flattened <- adder width t1Flattened t2Flattened
      assert $ m1 === m2
      assert $ (head t1Flattened /== head t2Flattened)
               || (head t1Flattened === head t2Flattened && head t1Flattened === head flattened) -- overflow detection
      return (flattened, m1 `union` m2)
    flatteningTermMUL (t1 :*: t2) = do
      undefined -- TODO
    flatteningTermNeg (Neg t) = do
      (tFlattened, m) <- flatteningTerm width t
      (one, _) <- flatteningTermINT (IConst 1)
      flattened <- adder width (map Ersatz.not tFlattened) one
      return (flattened, m)

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
