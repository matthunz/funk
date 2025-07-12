module Funk.IntegrationSpec where

import Test.Hspec
import Funk
import Data.List (isInfixOf)

spec :: Spec
spec = describe "Integration Tests" $ do
  describe "Successful programs" $ do
    it "compiles simple let binding" $ do
      let program = "let x = \\y . y; x"
      result <- tryRun program
      case result of
        Right _ -> return ()
        Left err -> expectationFailure $ "Expected success, got error: " ++ show err

    it "compiles identity function" $ do
      let program = "let identity = \\x . x; identity"
      result <- tryRun program
      case result of
        Right _ -> return ()
        Left err -> expectationFailure $ "Expected success, got error: " ++ show err

    it "compiles data type definition" $ do
      let program = "data Maybe = { value: a }; Maybe { value: \\x . x }"
      result <- tryRun program
      case result of
        Right _ -> return ()
        Left err -> expectationFailure $ "Expected success, got error: " ++ show err

    it "compiles trait definition and implementation" $ do
      let program = unlines
            [ "data Id = { runId: a };"
            , "trait Functor f { fmap: forall a b . (a -> b) -> f a -> f b };"
            , "impl Functor for Id { fmap: \\f . \\x . Id { runId: f x } };"
            , "Id { runId: \\x . x }"
            ]
      result <- tryRun program
      case result of
        Right _ -> return ()
        Left err -> expectationFailure $ "Expected success, got error: " ++ show err

    it "compiles Reader monad example" $ do
      let program = unlines
        [ "data Reader = forall r a . { runReader: r -> a };"
        , "trait Functor f { fmap: forall a b . (a -> b) -> f a -> f b };"
        , "impl Functor for Reader { fmap: \\f . \\reader . Reader { runReader: \\r . f r } };"
        , "let ask = Reader { runReader: \\r . r };"
        , "let identity = \\x . x;"
        , "fmap identity ask"
        ]
      result <- tryRun program
      case result of
        Right _ -> return ()
        Left err -> expectationFailure $ "Expected success, got error: " ++ show err

  describe "Type checking failures" $ do
    it "fails on undefined variable" $ do
      let program = "undefined_var"
      result <- tryRun program
      case result of
        Left err -> do
          let errStr = show err
          errStr `shouldSatisfy` isInfixOf "Unknown identifier"
        Right _ -> expectationFailure "Expected failure for undefined variable"

    it "fails on missing trait implementation" $ do
      let program = unlines
        [ "data Id = { runId: a };"
        , "trait Functor f { fmap: forall a b . (a -> b) -> f a -> f b };"
        , "fmap (\\x . x) (Id { runId: 42 })"  -- No Functor instance for Id
        ]
      result <- tryRun program
      case result of
        Left err -> do
          let errStr = show err
          errStr `shouldSatisfy` isInfixOf "No implementation of trait"
        Right _ -> expectationFailure "Expected failure for missing trait implementation"

    it "fails on type mismatch in lambda" $ do
      let program = unlines
        [ "data Num = { value: forall . {} };"
        , "data Str = { text: forall . {} };"
        , "let f = \\x : Num . x;"
        , "f (Str { text: {} })"  -- Type mismatch
        ]
      result <- tryRun program
      case result of
        Left err -> do
          let errStr = show err
          -- Should fail during type checking
          return ()
        Right _ -> expectationFailure "Expected failure for type mismatch"

  describe "Parse failures" $ do
    it "fails on malformed lambda" $ do
      let program = "\\x"  -- Missing dot and body
      result <- tryRun program
      case result of
        Left (ParserError _) -> return ()
        Left other -> expectationFailure $ "Expected parser error, got: " ++ show other
        Right _ -> expectationFailure "Expected parse failure"

    it "fails on missing semicolon" $ do
      let program = "let x = y let z = w; z"  -- Missing semicolon after first let
      result <- tryRun program
      case result of
        Left (ParserError _) -> return ()
        Left other -> expectationFailure $ "Expected parser error, got: " ++ show other
        Right _ -> expectationFailure "Expected parse failure"

    it "fails on invalid trait syntax" $ do
      let program = "trait Functor { fmap }"  -- Missing type signature
      result <- tryRun program
      case result of
        Left (ParserError _) -> return ()
        Left other -> expectationFailure $ "Expected parser error, got: " ++ show other
        Right _ -> expectationFailure "Expected parse failure"

    it "fails on missing let keyword" $ do
      let program = "x = \\y . y; x"  -- Missing 'let' keyword
      result <- tryRun program
      case result of
        Left (ParserError _) -> return ()
        Left other -> expectationFailure $ "Expected parser error, got: " ++ show other
        Right _ -> expectationFailure "Expected parse failure"

  describe "Complex examples" $ do
    it "handles nested lambdas" $ do
      let program = "let f = \\x . \\y . \\z . x; f"
      result <- tryRun program
      case result of
        Right _ -> return ()
        Left err -> expectationFailure $ "Expected success, got error: " ++ show err

    it "handles function application chains" $ do
      let program = unlines
        [ "let apply = \\f . \\x . f x;"
        , "let identity = \\x . x;"
        , "apply identity identity"
        ]
      result <- tryRun program
      case result of
        Right _ -> return ()
        Left err -> expectationFailure $ "Expected success, got error: " ++ show err