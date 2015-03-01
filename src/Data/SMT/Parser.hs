{-# Language DataKinds #-}
module Data.SMT.Parser where

import Data.SMT.Abstract.Types
import Data.SMT.BitBlasting.Types
import Data.Extensible.Sum

import Control.Applicative

import Text.Trifecta

-- Parsers of formulas

parseEqual :: Parser Term -> Parser Formula
parseEqual p = embed <$> ((:=:) <$> p <*> (token (char '=') >> p))

parseLessThan :: Parser Term -> Parser Formula
parseLessThan p = embed <$> ( try ((:<:) <$> p <*> (token (char '<') >> p)) <|> try (flip (:<:) <$> p <*> (token (char '>') >> p)))

parseLiteral :: Parser Term -> Parser Formula
parseLiteral p = try (parseEqual p) <|> try (parseLessThan p)

parseFormula :: Parser Formula
parseFormula = do
  literals <- (parseLiteral parseTerm) `sepBy` (token $ char '&')
  return $ (foldl1 (\t1 t2 -> embed (t1 :&: t2)) literals)

-- Parsers of terms

parseVar :: Parser Term
parseVar = embed <$> (Var . read <$> (char 'X' >> many digit))

parseIConst :: Parser Term
parseIConst = embed <$> (IConst . read <$> many digit)

parseAdd :: Parser Term -> Parser Term
parseAdd p = embed <$> ((:+:) <$> p <*> (token (char '+') >> p))

parseNeg :: Parser Term -> Parser Term
parseNeg p = embed <$> (Neg <$> (token (char '-') >> p))

parseAtom :: Parser Term ->  Parser Term
parseAtom p = token $ (parseNeg p)
              <|> (parens p)
              <|> parseVar
              <|> parseIConst

parseTerm :: Parser Term
parseTerm = do
  atoms <- (parseAtom parseTerm) `sepBy` (token $ char '+')
  return $ (foldl1 (\t1 t2 -> embed (t1 :+: t2)) atoms)
