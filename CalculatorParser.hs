module CalculatorParser where

import CalculatorDSL
import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Data.Text as T
import qualified Text.Megaparsec.Char.Lexer as L

-- the parser type
type Parser = Parsec Void Text

-- Consume spaces
spaceConsumer :: Parser ()
spaceConsumer =
    L.space
        space1
        (L.skipLineComment "//")
        (L.skipBlockComment "/*" "*/")

-- lexical element parser
lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

-- symbol parser
symbol :: Text -> Parser Text
symbol = L.symbol spaceConsumer

-- parentheses parser
parentheses :: Parser a -> Parser a
parentheses = between (symbol "(") (symbol ")")

-- list of reserved keywords
reservedKeywords :: [Text]
reservedKeywords = ["set", "unset", "assume", "assert", "notNaN"]

-- identifier parser that rejects reserved keywords
identifier :: Parser String
identifier = lexeme (p >>= checkReserved)
  where
    p = (:) <$> letterChar <*> many (alphaNumChar <|> char '_')
    checkReserved x
      | T.pack x `elem` reservedKeywords =
            fail $ "attempt to use keyword " ++ show x ++ " as an identifier"
      | otherwise                        = pure x

-- parse a number
number :: Parser Double
number =
    lexeme (try L.float <|> (fromIntegral <$> (L.decimal :: Parser Integer)))

-- table of operators, in order of precedence.
operatorTable :: [[Operator Parser (Expression Concrete)]]
operatorTable =
      [ [     InfixL (Multiply <$ symbol "*")
            , InfixL (Divide   <$ symbol "/") ]
    , [       InfixL (Add      <$ symbol "+")
            , InfixL (Subtract <$ symbol "-") ] ]

-- expression parser
expression :: Parser (Expression Concrete)
expression = makeExprParser term operatorTable

-- term parser
term :: Parser (Expression Concrete)
term =
        parentheses expression
    <|> (Literal <$> number)
    <|> (Variable <$> identifier)

-- parse a "not NaN" expression
notNaN :: Parser (RelationalExpression Concrete)
notNaN = NotNaN <$> (symbol "notNaN" *> (parentheses expression <|> term))

-- Parse a relational operation
relationalOperation ::
        Parser (Expression Concrete -> Expression Concrete
                    -> RelationalExpression Concrete)
relationalOperation =
    choice [
          Equal             <$ (symbol "==" <|> symbol "=")
        , NotEqual          <$ (symbol "!=" <|> symbol "/=")
        , LessThanEqual     <$ symbol "<="
        , GreaterThanEqual  <$ symbol ">="
        , LessThan          <$ symbol "<"
        , GreaterThan       <$ symbol ">"]

-- Parse a relational expression
relationalExpression :: Parser (RelationalExpression Concrete)
relationalExpression =
    notNaN <|> (relationalOperation <*> expression <*> expression)

-- Parse a set statement
setStatement :: Parser (Statement Concrete)
setStatement = Set <$> identifier <* symbol "=" <*> expression

-- Parse an unset statement
unsetStatement :: Parser (Statement Concrete)
unsetStatement = Unset <$> identifier

-- Parse an assume statement
assume :: Parser (Statement Concrete)
assume = Assume <$> relationalExpression

-- Parse an assert statement
parseAssert :: Parser (Statement Concrete)
parseAssert = Assert <$> relationalExpression

parseStatement :: Parser (Statement Concrete)
parseStatement =
    choice [
          setStatement
        , unsetStatement
        , assume
        , parseAssert ]
