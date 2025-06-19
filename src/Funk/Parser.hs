{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Funk.Parser where

import Control.Monad
import Data.List
import Funk.Lexer
import Funk.SExpr
import Funk.Term
import Text.Parsec (SourcePos)

type PType = Type (Located Ident)

newtype PBinding = PBinding {unPBinding :: Located Ident}
  deriving (Show, Eq)

instance Binding PBinding where
  type BTVar PBinding = Located Ident
  type BVar PBinding = PBinding
  type BLam PBinding = SourcePos
  type BApp PBinding = SourcePos
  type BTyLam PBinding = SourcePos
  type BTyApp PBinding = SourcePos

type PTerm = Term PBinding

data PError
  = PError String SourcePos
  deriving (Show, Eq)

sexprPos :: PSExpr -> SourcePos
sexprPos (SAtom i) = locatedPos i
sexprPos (SGroup exprs) = case exprs of
  [] -> error "Cannot get position of empty SGroup"
  (e : _) -> sexprPos e

parseTerm :: PSExpr -> Either PError PTerm
parseTerm = parseTerm' []

parseTerm' :: [PBinding] -> PSExpr -> Either PError PTerm
parseTerm' ctx = \case
  SAtom i ->
    let current = PBinding i
     in case find (\b -> unLocated (unPBinding b) == unLocated i) ctx of
          Just binder -> Right $ Var binder current
          Nothing -> Right $ Var current current
  SGroup exprs -> parseGroup ctx exprs

parseGroup :: [PBinding] -> [PSExpr] -> Either PError PTerm
parseGroup _ [] = Left $ PError "Unexpected empty S-Expression group '()'" (error "No position for empty group")
parseGroup ctx (headExpr : args) =
  case headExpr of
    SAtom f
      | unIdent (unLocated f) == "fn" -> parseLam f ctx args
      | unIdent (unLocated f) == "forall" -> parseForall f ctx args
    _ -> do
      f' <- parseTerm' ctx headExpr
      foldM
        ( \acc argSexpr -> do
            arg <- parseTerm' ctx argSexpr
            return $ App (sexprPos argSexpr) acc arg
        )
        f'
        args

parseLam :: Located Ident -> [PBinding] -> [PSExpr] -> Either PError PTerm
parseLam fnKeyword ctx [SGroup [SAtom x], bodySexpr] = do
  let binder = PBinding x
  body <- parseTerm' (binder : ctx) bodySexpr
  Right $ Lam (locatedPos fnKeyword) binder Nothing body
parseLam fnKeyword ctx [SGroup [SAtom x, tySexpr], bodySexpr] = do
  ty <- parseType tySexpr
  let binder = PBinding x
  body <- parseTerm' (binder : ctx) bodySexpr
  Right $ Lam (locatedPos fnKeyword) binder (Just ty) body
parseLam fnKeyword _ _ =
  Left $ PError "Invalid 'fn' syntax. Expected: (fn (x Type) body)" (locatedPos fnKeyword)

parseForall :: Located Ident -> [PBinding] -> [PSExpr] -> Either PError PTerm
parseForall forallKeyword ctx [SGroup tvSexprs, bodySexpr] = do
  tvs <- mapM expectAtom tvSexprs
  body <- parseTerm' ctx bodySexpr
  Right $ foldr (TyLam (locatedPos forallKeyword)) body tvs
  where
    expectAtom (SAtom i) = Right i
    expectAtom other = Left $ PError "Expected type variable atom in forall" (sexprPos other)
parseForall forallKeyword _ _ =
  Left $ PError "Invalid 'forall' syntax. Expected: (forall (t1 t2 ...) body)" (locatedPos forallKeyword)

parseType :: PSExpr -> Either PError PType
parseType (SAtom i) = Right $ TVar i
parseType (SGroup [SAtom arr, t1, t2])
  | unIdent (unLocated arr) == "fn" = TArrow <$> parseType t1 <*> parseType t2
parseType (SGroup [SAtom forAll, SGroup tvs, body])
  | unIdent (unLocated forAll) == "forall" = do
      tvNames <- mapM expectAtom tvs
      body' <- parseType body
      Right $ foldr TForall body' tvNames
  where
    expectAtom (SAtom i) = Right i
    expectAtom other = Left $ PError "Expected type variable atom in forall type" (sexprPos other)
parseType s = Left $ PError "Invalid type expression" (sexprPos s)
