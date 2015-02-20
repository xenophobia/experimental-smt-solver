module Data.SMT.Parser where

import Data.Char
import Data.SMT.Abstract.Types
import Data.SMT.BitBlasting.Types
import Data.List.Split
import Data.Extensible.Sum

-- Tempolary impl.
parseSMT :: String -> IO Formula
parseSMT str = return $ foldl1 (<:&:>) $ map aux eqs
  where eqs = splitOn "&" str
        aux :: String -> Formula
        aux eq = let (leftHand, _:rightHand) = span (/= '=') eq in
                 parseTerm leftHand <:=:> parseTerm rightHand
        parseTerm :: String -> Term
        parseTerm = foldl1 (<:+:>) . map (parseFact . filter (/= ' ')) . splitOn "+" 
        parseFact :: String -> Term
        parseFact fact | all isDigit fact = embed $ IConst (read fact :: Int)
                       | fact /= "" && head fact == 'X' = embed $ Var (read $ tail fact)
