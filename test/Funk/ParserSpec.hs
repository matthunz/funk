module Funk.ParserSpec where

import Funk.Parser
import Funk.Term
import Funk.Token
import Test.Hspec
import Text.Parsec (eof, parse)
import Text.Parsec.Pos (initialPos)

spec :: Spec
spec = describe "Parser Tests" $ do
  describe "Successful parsing" $ do
    it "parses simple variable" $ do
      let tokens = [Located (initialPos "") (TokIdent "x")]
      case parse (varExpr <* eof) "" tokens of
        Right (Var _ (PBinding (Located _ (Ident "x")))) -> return ()
        Right _ -> expectationFailure "Expected Var x, got something else"
        Left err -> expectationFailure $ "Parse error: " ++ show err

    it "parses lambda expression" $ do
      let tokens =
            [ Located (initialPos "") TokLambda,
              Located (initialPos "") (TokIdent "x"),
              Located (initialPos "") TokDot,
              Located (initialPos "") (TokIdent "x")
            ]
      case parse (lambdaExpr <* eof) "" tokens of
        Right (Lam _ (PBinding (Located _ (Ident "x"))) Nothing (Var _ (PBinding (Located _ (Ident "x"))))) -> return ()
        Right _ -> expectationFailure "Expected lambda, got something else"
        Left err -> expectationFailure $ "Parse error: " ++ show err

    it "parses let statement" $ do
      let tokens =
            [ Located (initialPos "") TokLet,
              Located (initialPos "") (TokIdent "x"),
              Located (initialPos "") TokEq,
              Located (initialPos "") (TokIdent "y"),
              Located (initialPos "") TokSemicolon
            ]
      case parse (letStmt <* eof) "" tokens of
        Right (Let _ (PBinding (Located _ (Ident "x"))) Nothing (Var _ (PBinding (Located _ (Ident "y"))))) -> return ()
        Right _ -> expectationFailure "Expected let statement, got something else"
        Left err -> expectationFailure $ "Parse error: " ++ show err

    it "parses trait definition" $ do
      let tokens =
            [ Located (initialPos "") TokTrait,
              Located (initialPos "") (TokIdent "Functor"),
              Located (initialPos "") (TokIdent "f"),
              Located (initialPos "") TokLBrace,
              Located (initialPos "") (TokIdent "fmap"),
              Located (initialPos "") TokColon,
              Located (initialPos "") (TokIdent "a"),
              Located (initialPos "") TokRBrace
            ]
      case parse (traitStmt <* eof) "" tokens of
        Right (Trait (Located _ (Ident "Functor")) _ _) -> return ()
        Right _ -> expectationFailure "Expected trait definition, got something else"
        Left err -> expectationFailure $ "Parse error: " ++ show err

  describe "Parse failures" $ do
    it "fails on incomplete lambda" $ do
      let tokens =
            [ Located (initialPos "") TokLambda,
              Located (initialPos "") (TokIdent "x")
            ]
      case parse (lambdaExpr <* eof) "" tokens of
        Left _ -> return () -- Expected failure
        Right _ -> expectationFailure "Expected parse failure, got success"

    it "fails on missing let keyword" $ do
      let tokens =
            [ Located (initialPos "") (TokIdent "x"),
              Located (initialPos "") TokEq,
              Located (initialPos "") (TokIdent "y"),
              Located (initialPos "") TokSemicolon
            ]
      case parse (letStmt <* eof) "" tokens of
        Left _ -> return () -- Expected failure
        Right _ -> expectationFailure "Expected parse failure, got success"

    it "fails on malformed trait definition" $ do
      let tokens =
            [ Located (initialPos "") TokTrait,
              Located (initialPos "") (TokIdent "Functor"),
              Located (initialPos "") TokLBrace
              -- Missing closing brace
            ]
      case parse (traitStmt <* eof) "" tokens of
        Left _ -> return () -- Expected failure
        Right _ -> expectationFailure "Expected parse failure, got success"
