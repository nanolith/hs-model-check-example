module CalculatorDSL where

data Expression =
      Literal Double
    | Variable String
    | Add Expression Expression
    | Multiplication Expression Expression
    | Divide Expression Expression
    deriving (Show, Eq)
