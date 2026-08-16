module CalculatorParser where

import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
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
