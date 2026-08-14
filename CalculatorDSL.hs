module CalculatorDSL where

import GHC.Generics (Generic)
import Grisette (SymBool, SymFP64)
import qualified Data.Map.Strict as Map

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

-- Data tags for supporting concrete and symbolic interpretation
data Concrete
data Symbolic

type family HKD f a where
    HKD Concrete a = a
    HKD Symbolic Double = SymDouble
    HKD Symbolic Bool = SymBool

-- Calculator runtime state
data CalculatorState f = CalculatorState {
      env :: Map.Map String (HKD f Double)
    , assertions :: HKD f Bool
    , safeDivideConditional :: HKD f Bool
    } deriving (Generic)

deriving instance (Show (HKD f Double), Show (HKD f Bool)) =>
    Show (CalculatorState f)
