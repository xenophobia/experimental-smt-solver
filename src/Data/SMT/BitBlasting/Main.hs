{-# Language LambdaCase #-}
module Main where

import Data.Monoid
import Control.Applicative
import Text.Trifecta
import Data.SMT.Abstract.Types
import Data.SMT.Parser
import Data.SMT.BitBlasting.Solver
import Data.SMT.Solution
import qualified Data.IntMap as IM

defaultThreshold :: Int
defaultThreshold = 10

incrementalTrial :: Monad m => Int -> Int -> (Int -> m Solution) -> m Solution
incrementalTrial i n act | i >= n = act i
                         | otherwise = do
                             act i >>= \case
                               answer@(Satisfied _) -> return $ answer
                               _ -> incrementalTrial (i+1) n act

main :: IO ()
main = do
  input <- getLine
  case parseString (parseFormula <* eof) mempty input of
    Failure d -> print d
    Success parsed -> do
      putStrLn $ "Constraint: " ++ ppr parsed
      incrementalTrial 2 defaultThreshold (flip bitblasting parsed) >>= \case
        Unknown -> putStrLn "Failed to solve"
        Unsatisfied -> putStrLn $ "Unsatisfiable (in threshold = " ++ show defaultThreshold ++ ")"
        Satisfied ans -> putStrLn $ "Satisfiable by: " ++ concat (map pr (IM.toList ans))
          where pr (i, n) = '\n' : 'X' : show i ++ " = " ++ show n
