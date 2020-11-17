module Lambda where

import           Text.ParserCombinators.Parsec          (Parser, alphaNum, char,
                                                         many1, parse, (<|>))
import           Text.ParserCombinators.Parsec.Language (emptyDef)
import qualified Text.ParserCombinators.Parsec.Token    as Token

type Symb = String

infixl 2 :@

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
