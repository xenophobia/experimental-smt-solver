{-# Language LambdaCase #-}
module BitBlastingSpec where

import Data.Monoid
import Text.Trifecta
import Data.SMT.BitBlasting.Parser
import Data.SMT.BitBlasting.Types
import Data.SMT.Abstract.Types
import Data.SMT.BitBlasting.Solver
import Data.SMT.Solution
import Data.SMT.Util
import Control.Applicative

import Test.Hspec

inputs :: [String]
inputs = [ "X1=0"
         , "0=0"
         , "X1=X2"
         , "X1+X2=2 & X1+(-2*X2)=5"
         , "X1 * X2 * X3 = 30 & X1 + X2 + X3 = 0 & X1 < X2 & X2 < X3"
         , "X1 * X1 + X2 * X2 = X3 * X3 & 10 < X1 & X1 < X2 & X2 < X3"
         ]

spec :: Spec
spec = do
  describe "Satisfiable with" $ do
    flip mapM_ inputs $ \input -> do
      it input $ do
        case parseString (parseFormula <* eof) mempty input of
          Failure d -> expectationFailure (show d)
          Success parsed -> solve defaultConfig parsed >>= \case
            Satisfied ans -> do
              putStrLn $ "solve(" ++ ppr parsed ++ ")=" ++ show ans
              (eval ans parsed) `shouldBe` True
            _ -> expectationFailure $ input ++ " is satisfiable but verification failure occured."
