module CalculatorParser where

import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec

-- the parser type
type Parser = Parsec Void Text
