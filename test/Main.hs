module Main where

import Data.Either
import Funk (showErrorPretty, tryRun)
import Funk.SExpr (sExprPretty)
import Funk.Solver (sTermToSExpr)
import Test.Hspec

run :: String -> IO ()
run input = do
  res <- tryRun input
  res' <- case res of
    Right t -> do
      expr <- sTermToSExpr t
      return . Right $ sExprPretty expr
    Left e -> do
      eStr <- showErrorPretty e input
      return $ Left eStr
  res' `shouldSatisfy` isRight

main :: IO ()
main = hspec $ do
  describe "Prelude.head" $ do
    it "compiles true" $ run "(forall (a) (fn (x a) (fn (y a) x)))"
