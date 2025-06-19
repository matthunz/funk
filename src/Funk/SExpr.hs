{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}

module Funk.SExpr where

import Text.Parsec

data Located a = Located
  { locatedPos :: SourcePos,
    unLocated :: a
  }
  deriving (Eq, Functor)

instance (Show a) => Show (Located a) where
  show (Located _ a) = show a

data SExpr b
  = SAtom b
  | SGroup [SExpr b]
  deriving (Show, Eq)

sExprPretty :: SExpr String -> String
sExprPretty = \case
  SAtom b -> b
  SGroup exprs -> "(" ++ unwords (map sExprPretty exprs) ++ ")"
