module Funk where

import Data.List
import Funk.Lexer (parseSExpr)
import Funk.Parser (PError (..), parseTerm)
import Funk.SExpr
import Funk.Solver
import Funk.Term
import Options.Applicative hiding (ParseError)
import System.Console.ANSI
import Text.Parsec
import Text.Parsec.Error

newtype Options = Options
  { optionsFilePath :: FilePath
  }

options :: Parser Options
options =
  Options <$> argument str (metavar "FILE" <> help "Path to the input file")

run :: IO ()
run = do
  opts <- execParser $ info (options <**> helper) fullDesc
  input <- readFile (optionsFilePath opts)
  res <- tryRun input
  case res of
    Left err -> showErrorPretty err input >>= putStrLn
    Right st -> do
      stExpr <- sTermToSExpr st
      putStrLn $ sExprPretty stExpr

tryRun :: String -> IO (Either Error STerm)
tryRun input = case parseSExpr "<stdin>" input of
  Left err -> return $ Left (LexerError err)
  Right sexpr -> case parseTerm sexpr of
    Left err -> return $ Left (ParserError err)
    Right term -> do
      res <- solvePTerm term
      case res of
        Left errs -> return $ Left (SolverError errs)
        Right st -> return $ Right st

data Error = LexerError ParseError | ParserError PError | SolverError [SError]

showLexerErrorPretty :: ParseError -> String -> String
showLexerErrorPretty err input =
  let (msgs, unexpect, expects) =
        foldl
          ( \(m, u, e) msg' ->
              case msg' of
                Message m' -> (m ++ [m'], u, e)
                UnExpect s -> (m, s, e)
                Expect s -> (m, u, e ++ [s])
                SysUnExpect s -> (m, s, e)
          )
          ([], "", [])
          (errorMessages err)
      expecting = case expects of
        [] -> "unexpected token"
        [x] -> "expecting " ++ x
        xs ->
          "expecting "
            ++ intercalate ", " (reverse . tail $ reverse xs)
            ++ " or "
            ++ last xs
      msg =
        expecting
          ++ ", found "
          ++ if null unexpect
            then "<end of input>"
            else
              unexpect
                ++ if null msgs
                  then ""
                  else
                    setSGRCode [SetColor Foreground Vivid Red]
                      ++ "help:\n"
                      ++ setSGRCode [Reset]
                      ++ intercalate "\n" msgs
      pos = errorPos err
      pos' = setSourceColumn pos (sourceColumn pos + 1)
   in showErrorLine pos' input msg

showParserErrorPretty :: PError -> String -> String
showParserErrorPretty (PError msg pos) input =
  let pos' = setSourceColumn pos (sourceColumn pos + 1)
   in showErrorLine pos' input msg

showSErrorPretty :: SError -> String -> IO String
showSErrorPretty err input =
  case err of
    MissingIdent i ->
      return $
        showErrorLine (locatedPos i) input $
          "Unknown identifier `" ++ unIdent (unLocated i) ++ "`"
    InfiniteType i -> case i of
      Left pos -> return $ showErrorLine pos input "Infinite type detected"
      Right ident ->
        return $
          showErrorLine (locatedPos ident) input $
            "Infinite type: `" ++ unIdent (unLocated ident) ++ "`"
    UnificationError t1 t2 -> do
      t1Expr <- sTypeToSExpr t1
      t2Expr <- sTypeToSExpr t2
      return $
        "Unification error: cannot unify types `"
          ++ sExprPretty t1Expr
          ++ "` and `"
          ++ sExprPretty t2Expr
          ++ "`"

showErrorPretty :: Error -> String -> IO String
showErrorPretty (LexerError err) input = return $ showLexerErrorPretty err input
showErrorPretty (ParserError err) input = return $ showParserErrorPretty err input
showErrorPretty (SolverError errs) input = unlines <$> mapM (`showSErrorPretty` input) errs

showErrorLine :: SourcePos -> String -> String -> String
showErrorLine pos input msg =
  let linePos = sourceLine pos
      colPos = sourceColumn pos
      srcLine = case drop (linePos - 1) (lines input) of
        (line : _) -> line
        [] -> ""
   in unlines
        [ " --> "
            ++ sourceName pos
            ++ ":"
            ++ show linePos
            ++ ":"
            ++ show colPos,
          "  |",
          show linePos ++ " | " ++ srcLine,
          "  |"
            ++ setSGRCode [SetColor Foreground Vivid Red]
            ++ replicate colPos ' '
            ++ "^ "
            ++ msg
            ++ setSGRCode [Reset]
        ]
