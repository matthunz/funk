module Funk.SimpleSpec where

import Funk
import Test.Hspec

spec :: Spec
spec = describe "Simple Integration Tests" $ do
  describe "Successful compilation" $ do
    it "compiles identity function" $ do
      let program = "let identity = \\x . x; identity"
      result <- tryRun program
      case result of
        Right _ -> return ()
        Left err -> expectationFailure $ "Expected success, got error: " ++ show err

    it "compiles simple let binding" $ do
      let program = "let x = \\y . y; x"
      result <- tryRun program
      case result of
        Right _ -> return ()
        Left err -> expectationFailure $ "Expected success, got error: " ++ show err

    it "compiles complex lambda expressions" $ do
      let program = "let f = \\x . \\y . \\z . x; f"
      result <- tryRun program
      case result of
        Right _ -> return ()
        Left err -> expectationFailure $ "Expected success, got error: " ++ show err

  describe "Expected failures" $ do
    it "fails on undefined variable" $ do
      let program = "undefined_var"
      result <- tryRun program
      case result of
        Left (SubstError _) -> return () -- Expected: substitution error for unknown identifier
        Left _ -> expectationFailure "Expected SubstError for undefined variable"
        Right _ -> expectationFailure "Expected failure for undefined variable"

    it "fails on malformed lambda" $ do
      let program = "\\x" -- Missing dot and body
      result <- tryRun program
      case result of
        Left (ParserError _) -> return ()
        Left other -> expectationFailure $ "Expected parser error, got: " ++ show other
        Right _ -> expectationFailure "Expected parse failure"

    it "fails on missing let keyword" $ do
      let program = "x = \\y . y; x" -- Missing 'let' keyword
      result <- tryRun program
      case result of
        Left (ParserError _) -> return ()
        Left other -> expectationFailure $ "Expected parser error, got: " ++ show other
        Right _ -> expectationFailure "Expected parse failure"
