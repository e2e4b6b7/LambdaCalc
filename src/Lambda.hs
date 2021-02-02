{-# LANGUAGE FlexibleContexts #-}

module Lambda where

import           Control.Monad.Except                   (MonadError (throwError))
import qualified Data.HashMap.Strict                    as Map
import           Data.Maybe                             (fromMaybe)
import           Text.ParserCombinators.Parsec          (Parser, alphaNum, char,
                                                         many1, parse, (<|>))
import           Text.ParserCombinators.Parsec.Language (emptyDef)
import qualified Text.ParserCombinators.Parsec.Token    as Token

type Map = Map.HashMap

infixl 4 :@

infixr 3 :->

type Symb = String

-- Терм
data Expr
  = Var Symb
  | Expr :@ Expr
  | Lam Symb Expr
  deriving (Eq)

instance Show Expr where
  showsPrec _ (Var s) = showString s
  showsPrec _ (e :@ arg) = shows e . showChar ' ' . showParen True (shows arg)
  showsPrec _ (Lam v e) =
    showParen True $ showChar '\\' . showString v . showString " -> " . shows e

instance Read Expr where
  readsPrec _ inp = isParsed $ parse term "" inp
    where
      isParsed :: Either b a -> [(a, String)]
      isParsed (Right a) = [(a, "")]
      isParsed _         = []
      languageDef =
        emptyDef
          { Token.identLetter = alphaNum <|> char '_' <|> char '\''
          , Token.reservedOpNames = ["\\", "->"]
          }
      lexer = Token.makeTokenParser languageDef
      reservedOp = Token.reservedOp lexer
      brackets = Token.parens lexer
      identifier :: Parser Symb
      identifier = Token.identifier lexer
      term :: Parser Expr
      term = foldl1 (:@) <$> many1 item
      item :: Parser Expr
      item = brackets term <|> lambda <|> variable
      lambda :: Parser Expr
      lambda = do
        reservedOp "\\"
        vars <- many1 identifier
        reservedOp "->"
        expr <- term
        return $ foldr Lam expr vars
    --lambda = flip (foldr Lam) <$ reservedOp "\\" <*> many1 identifier <* reservedOp "->" <*> term
      variable :: Parser Expr
      variable = Var <$> identifier

-- Тип
data Type
  = TVar Symb
  | Type :-> Type
  deriving (Eq, Show)

-- Контекст
newtype Env =
  Env (Map Symb Type)
  deriving (Eq, Show)

appEnv :: MonadError String m => Env -> Symb -> m Type
appEnv (Env env) v =
  case Map.lookup v env of
    Just t -> return t
    Nothing ->
      throwError $ "There is no variable " ++ show v ++ " in the enviroment."

extendEnv :: Env -> Symb -> Type -> Env
extendEnv (Env env) s t = Env $ Map.insert s t env

-- Подстановка
newtype SubsTy =
  SubsTy (Map Symb Type)
  deriving (Eq, Show)

appSubsTy :: SubsTy -> Type -> Type
appSubsTy (SubsTy subsMap) st@(TVar s) = fromMaybe st (Map.lookup s subsMap)
appSubsTy subs (t1 :-> t2) = appSubsTy subs t1 :-> appSubsTy subs t2

appSubsEnv :: SubsTy -> Env -> Env
appSubsEnv subs (Env env) = Env $ fmap (appSubsTy subs) env

instance Semigroup SubsTy where
  s1@(SubsTy subs1) <> SubsTy subs2 =
    SubsTy $ Map.union (fmap (appSubsTy s1) subs2) subs1

instance Monoid SubsTy where
  mempty = SubsTy Map.empty
