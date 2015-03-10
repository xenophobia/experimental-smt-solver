{-# Language UnicodeSyntax #-}
{-# Language TypeOperators #-}
{-# Language DataKinds #-}
{-# Language FlexibleContexts #-}
module Data.SMT.BitBlasting.Parser where

import Data.SMT.Parser
import Data.SMT.BitBlasting.Types
import Data.Extensible

import Control.Applicative

import Text.Trifecta

-- Parsers of formulas

parseLiteral :: Parser Term -> Parser Formula
parseLiteral p = try (parseEqual p) <|> try (parseLessThan p)

parseFormula :: Parser Formula
parseFormula = parseAnd (parseLiteral parseTerm)

-- Parsers of terms

parseAtom = token $ (parseNeg parseFact)
            <|> (parens parseTerm)
            <|> parseVar
            <|> parseIConst

parseFact :: Parser Term
parseFact = parseMul parseAtom

parseTerm :: Parser Term
parseTerm = parseAdd parseFact
