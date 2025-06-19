module Funk.Lexer where

import Funk.SExpr
import Funk.Term
import Text.Parsec
import Text.Parsec.String

type PSExpr = SExpr (Located Ident)

atom :: Parser (Located Ident)
atom = do
  pos <- getPosition
  firstChar <- letter <|> char '_'
  rest <- many $ alphaNum <|> char '_' <|> char '-'
  return . Located pos $ Ident (firstChar : rest)

group :: Parser PSExpr
group = do
  exprs <- between (char '(') (char ')') sexprs
  return $ SGroup exprs

sexprs :: Parser [PSExpr]
sexprs = many (try sexpr <* spaces)

sexpr :: Parser PSExpr
sexpr = group <|> (SAtom <$> atom)

parseSExpr :: String -> String -> Either ParseError PSExpr
parseSExpr = parse (spaces *> fmap SGroup sexprs <* eof)
