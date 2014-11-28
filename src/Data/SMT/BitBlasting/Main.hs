{-# Language LambdaCase #-}
module Main where

import Data.SMT.Parser
import Data.SMT.BitBlasting.Solver
import Data.SMT.Solution

defaultThreshold :: Int
defaultThreshold = 10

incrementalTrial :: Monad m => Int -> Int -> (Int -> m (Maybe SMTSolution)) -> m (Maybe SMTSolution)
incrementalTrial i n act | i >= n = act i
                         | otherwise = do
                             act i >>= \case
                               Just answer@(Satisfied _) -> return $ Just answer
                               _ -> incrementalTrial (i+1) n act

main :: IO ()
main = do
  input <- getLine
  parsed <- parseSMT input
  putStrLn $ "Constraint: " ++ show parsed
  incrementalTrial 2 defaultThreshold (flip bitblasting parsed) >>= \case
    Nothing -> putStrLn "Failed to solve"
    Just Unsatisfied -> putStrLn $ "Unsatisfiable (in threshold = " ++ show defaultThreshold ++ ")"
    Just (Satisfied ans) -> putStrLn $ "Satisfiable by: " ++ show ans
