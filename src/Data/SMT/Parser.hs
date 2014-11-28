module Data.SMT.Parser where

import Data.Char
import Data.SMT.Types
import Data.List.Split

-- Tempolary impl.
parseSMT :: String -> IO SMT
parseSMT str = return $ foldl1 (:&:) $ map aux eqs
  where eqs = splitOn "&" str
        aux eq = let (leftHand, _:rightHand) = span (/= '=') eq in
                 parseTerm leftHand :=: parseTerm rightHand
        parseTerm = foldl1 (:+:) . map (parseFact . filter (/= ' ')) . splitOn "+"
        parseFact fact | all isDigit fact = Const (read fact :: Int)
                       | fact /= "" && head fact == 'X' = Var (read $ tail fact)
