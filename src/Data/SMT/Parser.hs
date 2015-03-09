{-# Language UnicodeSyntax #-}
{-# Language TypeOperators #-}
{-# Language DataKinds #-}
{-# Language FlexibleContexts #-}
module Data.SMT.Parser where

import Data.SMT.Abstract.Types
import Data.Extensible

import Control.Applicative

import Text.Trifecta

-- Parsers of formulas

parseEqual :: (EQUAL ∈ cs) => Parser term -> Parser (AbstFormula term cs :| cs)
parseEqual p = embed <$> ((:=:) <$> p <*> (token (char '=') >> p))

parseLessThan :: (LESSTHAN ∈ cs) => Parser term -> Parser (AbstFormula term cs :| cs)
parseLessThan p = embed <$> ( try ((:<:) <$> p <*> (token (char '<') >> p)) <|> try (flip (:<:) <$> p <*> (token (char '>') >> p)))

parseAnd :: (AND ∈ cs) => Parser (AbstFormula term cs :| cs) -> Parser (AbstFormula term cs :| cs)
parseAnd p = foldl1 (\t1 t2 -> embed (t1 :&: t2)) <$> p `sepBy` (token $ char '&')

parseOr :: (OR ∈ cs) => Parser (AbstFormula term cs :| cs) -> Parser (AbstFormula term cs :| cs)
parseOr p = foldl1 (\t1 t2 -> embed (t1 :|: t2)) <$> p `sepBy` (token $ char '|')

-- Parsers of terms

parseVar :: (VAR ∈ cs) => Parser (AbstTerm cs :| cs)
parseVar = embed <$> (Var . read <$> (char 'X' >> many digit))

parseIConst :: (INT ∈ cs) => Parser (AbstTerm cs :| cs)
parseIConst = embed <$> (IConst . read <$> many digit)

parseNeg :: (NEG ∈ cs) => Parser (AbstTerm cs :| cs) -> Parser (AbstTerm cs :| cs)
parseNeg p = embed <$> (Neg <$> try (token (char '-') >> p))

parseAdd :: (ADD ∈ cs) => Parser (AbstTerm cs :| cs) -> Parser (AbstTerm cs :| cs)
parseAdd p = foldl1 (\t1 t2 -> embed (t1 :+: t2)) <$> p `sepBy` (token $ char '+')

parseMul :: (MUL ∈ cs) => Parser (AbstTerm cs :| cs) -> Parser (AbstTerm cs :| cs)
parseMul p = foldl1 (\t1 t2 -> embed (t1 :*: t2)) <$> p `sepBy` (token $ char '*')
