{-# Language LambdaCase #-}
module Main where

import Data.Monoid
import Control.Applicative
import Text.Trifecta
import Data.SMT.Abstract.Types
import Data.SMT.BitBlasting.Parser
import Data.SMT.BitBlasting.Types
import Data.SMT.BitBlasting.Solver
import Data.SMT.Solution
import qualified Data.IntMap as IM

main :: IO ()
main = do
  input <- getLine
  case parseString (parseFormula <* eof) mempty input of
    Failure d -> print d
    Success parsed -> do
      putStrLn $ "Constraint: " ++ ppr parsed
      solve defaultConfig parsed >>= \case
        Unknown -> putStrLn "Failed to solve"
        Unsatisfied -> putStrLn $ "Unsatisfiable (in threshold = " ++ show (maxWidth defaultConfig) ++ ")"
        Satisfied ans -> putStrLn $ "Satisfiable by: " ++ concat (map pr (IM.toList ans))
          where pr (i, n) = '\n' : 'X' : show i ++ " = " ++ show n
