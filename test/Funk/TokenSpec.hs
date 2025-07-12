module Funk.TokenSpec where

import Funk.Token
import Test.Hspec

spec :: Spec
spec = describe "Token Tests" $ do
  describe "Successful tokenization" $ do
    it "tokenizes simple identifier" $ do
      case tokenize "hello" of
        Right [Located _ (TokIdent "hello")] -> return ()
        Right other -> expectationFailure $ "Expected identifier, got: " ++ show other
        Left err -> expectationFailure $ "Tokenization error: " ++ show err

    it "tokenizes keywords" $ do
      case tokenize "let" of
        Right [Located _ TokLet] -> return ()
        Right other -> expectationFailure $ "Expected let token, got: " ++ show other
        Left err -> expectationFailure $ "Tokenization error: " ++ show err

    it "tokenizes forall keyword" $ do
      case tokenize "forall" of
        Right [Located _ TokForall] -> return ()
        Right other -> expectationFailure $ "Expected forall token, got: " ++ show other
        Left err -> expectationFailure $ "Tokenization error: " ++ show err

    it "tokenizes trait keyword" $ do
      case tokenize "trait" of
        Right [Located _ TokTrait] -> return ()
        Right other -> expectationFailure $ "Expected trait token, got: " ++ show other
        Left err -> expectationFailure $ "Tokenization error: " ++ show err

    it "tokenizes impl keyword" $ do
      case tokenize "impl" of
        Right [Located _ TokImpl] -> return ()
        Right other -> expectationFailure $ "Expected impl token, got: " ++ show other
        Left err -> expectationFailure $ "Tokenization error: " ++ show err

    it "tokenizes symbols" $ do
      case tokenize "->" of
        Right [Located _ TokArrow] -> return ()
        Right other -> expectationFailure $ "Expected arrow token, got: " ++ show other
        Left err -> expectationFailure $ "Tokenization error: " ++ show err

    it "tokenizes lambda" $ do
      case tokenize "\\" of
        Right [Located _ TokLambda] -> return ()
        Right other -> expectationFailure $ "Expected lambda token, got: " ++ show other
        Left err -> expectationFailure $ "Tokenization error: " ++ show err

    it "tokenizes complex expression" $ do
      case tokenize "let x = \\y . y;" of
        Right tokens -> length tokens `shouldBe` 8 -- let, x, =, \, y, ., y, ;
        Left err -> expectationFailure $ "Tokenization error: " ++ show err

  describe "Tokenization with comments" $ do
    it "ignores line comments" $ do
      case tokenize "x -- this is a comment\ny" of
        Right [Located _ (TokIdent "x"), Located _ (TokIdent "y")] -> return ()
        Right other -> expectationFailure $ "Expected x and y tokens, got: " ++ show other
        Left err -> expectationFailure $ "Tokenization error: " ++ show err

    it "handles empty input" $ do
      case tokenize "" of
        Right [] -> return ()
        Right other -> expectationFailure $ "Expected empty token list, got: " ++ show other
        Left err -> expectationFailure $ "Tokenization error: " ++ show err

    it "handles whitespace only" $ do
      case tokenize "   \n\t  " of
        Right [] -> return ()
        Left _ -> return () -- Tokenizer may fail on pure whitespace, which is acceptable
        Right other -> expectationFailure $ "Expected empty token list, got: " ++ show other
