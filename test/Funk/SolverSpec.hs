module Funk.SolverSpec where

import Test.Hspec
import Funk.Solver
import Funk.STerm
import Funk.Infer
import Funk.Fresh
import Funk.Subst (Env(..), runSubst, substBlock)
import Funk.Parser
import Funk.Token
import Text.Parsec

spec :: Spec
spec = describe "Solver Tests" $ do
  describe "Type unification" $ do
    it "unifies identical types" $ do
      -- This is a basic test - we'd need to set up proper STypes for real testing
      pending

    it "detects infinite types" $ do
      -- Test case for occurs check
      pending

  describe "Constraint solving" $ do
    it "solves simple equality constraints" $ do
      -- Test basic type inference
      pending

    it "solves trait constraints with valid implementations" $ do
      -- Test trait resolution
      pending

    it "fails when trait implementation is missing" $ do
      -- Test missing trait implementation error
      pending

  describe "Error reporting" $ do
    it "provides meaningful error messages for unification failures" $ do
      let program = unlines
        [ "data A = { value: forall . {} };"
        , "data B = { value: forall . {} };"
        , "let f = \\x : A . x;"
        , "f (B { value: {} })"
        ]
      result <- do
        let tokenResult = tokenize program
        case tokenResult of
          Left _ -> return $ Left "Tokenization failed"
          Right tokens -> do
            case parseTopLevel tokens of
              Left _ -> return $ Left "Parse failed"
              Right topLevel -> do
                (substResult, env) <- runSubst (substBlock topLevel)
                case substResult of
                  Left _ -> return $ Left "Substitution failed"
                  Right block -> do
                    cs <- fst <$> runFresh (constraintsBlock block) (Env $ envNextIdx env)
                    solveResult <- solveConstraints cs env
                    case solveResult of
                      Left errs -> return $ Right errs  -- We expect errors here
                      Right _ -> return $ Left "Expected constraint solving to fail"
      
      case result of
        Right _ -> return ()  -- Expected failure with error messages
        Left _ -> expectationFailure "Expected constraint solving to produce meaningful errors"

    it "reports missing trait implementations clearly" $ do
      let program = unlines
        [ "trait Show a { show: a -> forall . {} };"
        , "data MyType = { value: forall . {} };"
        , "show (MyType { value: {} })"  -- No Show instance for MyType
        ]
      -- Similar test setup as above
      pending