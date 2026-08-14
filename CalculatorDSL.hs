module CalculatorDSL where

-- Arithmetic expressions
data Expression =
      Literal Double
    | Variable String
    | Add Expression Expression
    | Multiplication Expression Expression
    | Divide Expression Expression
    deriving (Eq, Show)
