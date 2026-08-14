module CalculatorDSL where

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
