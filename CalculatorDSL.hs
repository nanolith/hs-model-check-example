module CalculatorDSL where

import Grisette (SymFP64)

-- Arithmetic expressions
data Expression =
      Literal Double
    | Variable String
    | Add Expression Expression
    | Multiplication Expression Expression
    | Divide Expression Expression
    deriving (Eq, Show)

-- Relational expressions
data RelationalExpression =
      Equal Expression Expression
    | NotEqual Expression Expression
    | LessThan Expression Expression
    | LessThanEqual Expression Expression
    | GreaterThan Expression Expression
    | GreaterThanEqual Expression Expression
    deriving (Eq, Show)

-- Statements
data Statement =
      Set String Expression
    | Unset String
    | Assert RelationalExpression
    deriving (Eq, Show)

-- Program
data Program = Program [Statement]
    deriving (Eq, Show)

-- Double symbolic type
type SymDouble = SymFP64
