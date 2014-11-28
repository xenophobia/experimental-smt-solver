module Data.SMT.Parser where

import Data.Char
import Data.SMT.Types
import Data.List.Split

-- Tempolary impl.
parseSMT :: String -> IO SMT
parseSMT str = return $ parseTerm leftHand :=: parseTerm rightHand
  where (leftHand, _:rightHand) = span (/= '=') str
        parseTerm = foldl1 (:+:) . map (parseFact . filter (/= ' ')) . splitOn "+"
        parseFact fact | all isDigit fact = Const (read fact :: Int)
                       | fact /= "" && head fact == 'X' = Var (read $ tail fact)
