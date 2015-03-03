{-# Language ViewPatterns #-}
{-# Language GADTs #-}
{-# LAnguage TypeOperators #-}
{-# Language DataKinds #-}
{-# Language ScopedTypeVariables #-}
{-# Language RecordWildCards #-}
{-# Language LambdaCase #-}

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

decodeToSigned :: [Bool] -> Int
decodeToSigned [] = undefined
decodeToSigned (map (\x -> if x then 1 else 0) -> b:bs) = 
  foldl (\acc n -> acc*2+n) 0 (-b:bs)

incrementalTrial :: Monad m => Int -> Int -> (Int -> m Solution) -> m Solution
incrementalTrial i n act | i >= n = act i
                         | otherwise = do
                             act i >>= \case
                               answer@(Satisfied _) -> return $ answer
                               _ -> incrementalTrial (i+1) n act

solve :: Config -> Formula -> IO Solution
solve Config{..} fm = incrementalTrial startWidth maxWidth (flip solveIn fm)

solveIn :: Int -> Formula -> IO Solution
solveIn width _fm = do
  (result, ~(Just answer)) <- minisat `solveWith` flattened
  case result of
    Ersatz.Unsolved -> return Unknown
    Ersatz.Unsatisfied -> return Unsatisfied
    Ersatz.Satisfied -> return $ Satisfied (IM.map decodeToSigned answer)
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
      (t1Flattened, m1) <- flatteningTerm width t1
      (t2Flattened, m2) <- flatteningTerm width t2
      let p_ = map (\a -> (\(b:bs) -> not b:bs) . map (a &&) $ t2Flattened) t1Flattened
          pk = true : p_!!(width-1)
          p0 = true : (map not $ p_!!0)
          p = p0 : (tail . init $ p_) ++ [pk]
          pShifted = map (\(f,bs,l) -> replicate f false ++ bs ++ replicate l false) $ zip3 (0 : [2 .. width-1] ++ [width-1]) p [width-1, width-2 ..0]
      flattened_ <- foldM (adder (width * 2)) (head pShifted) (tail pShifted)
      let (ofs, flattened@(sig:_)) = splitAt width flattened_
      assert $ Ersatz.all (=== sig) ofs
      assert $ m1 === m2
      return (flattened, m1 `union` m2)
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
