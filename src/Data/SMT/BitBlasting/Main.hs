{-# Language LambdaCase #-}
module Main where

import Data.SMT.Parser
import Data.SMT.BitBlasting.Solver
import Data.SMT.Solution

defaultThreshold :: Int
defaultThreshold = 3

main :: IO ()
main = do
  input <- getLine
  parsed <- parseSMT input
  putStrLn $ "Constraint: " ++ show parsed
  bitblasting defaultThreshold parsed >>= \case
    Nothing -> putStrLn "Failed to solve"
    Just Unsatisfied -> putStrLn $ "Unsatisfiable (in threshold = " ++ show defaultThreshold ++ ")"
    Just (Satisfied ans) -> putStrLn $ "Satisfiable by: " ++ show ans
