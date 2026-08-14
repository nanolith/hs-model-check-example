module CalculatorDSL where

import GHC.Generics (Generic)
import Grisette (SymBool, SymFP64)
import qualified Data.Map.Strict as Map

-- Double symbolic type
type SymDouble = SymFP64

-- Data tags for supporting concrete and symbolic interpretation
data Concrete
data Symbolic

type family HKD f a where
    HKD Concrete a = a
    HKD Symbolic Double = SymDouble
    HKD Symbolic Bool = SymBool

-- Arithmetic expressions
data Expression f =
      Literal (HKD f Double)
    | Variable String
    | Add (Expression f) (Expression f)
    | Multiplication (Expression f) (Expression f)
    | Divide (Expression f) (Expression f)

deriving instance (Eq (HKD f Double)) => Eq (Expression f)
deriving instance (Show (HKD f Double)) => Show (Expression f)

-- Relational expressions
data RelationalExpression f =
      Equal (Expression f) (Expression f)
    | NotEqual (Expression f) (Expression f)
    | LessThan (Expression f) (Expression f)
    | LessThanEqual (Expression f) (Expression f)
    | GreaterThan (Expression f) (Expression f)
    | GreaterThanEqual (Expression f) (Expression f)

deriving instance (Eq (Expression f)) => Eq (RelationalExpression f)
deriving instance (Show (Expression f)) => Show (RelationalExpression f)

-- Statements
data Statement f =
      Set String (Expression f)
    | Unset String
    | Assert (RelationalExpression f)

deriving instance (Eq (Expression f), Eq (RelationalExpression f)) =>
    Eq (Statement f)
deriving instance (Show (Expression f), Show (RelationalExpression f)) =>
    Show (Statement f)

-- Program
data Program f = Program [Statement f]

deriving instance (Eq (Statement f)) => Eq (Program f)
deriving instance (Show (Statement f)) => Show (Program f)

-- Calculator runtime state
data CalculatorState f = CalculatorState {
      env :: Map.Map String (HKD f Double)
    , assertions :: HKD f Bool
    , safeDivideConditional :: HKD f Bool
    } deriving (Generic)

deriving instance (Eq (HKD f Double), Eq (HKD f Bool)) =>
    Eq (CalculatorState f)
deriving instance (Show (HKD f Double), Show (HKD f Bool)) =>
    Show (CalculatorState f)

-- Symbolic and Runtime states
type RuntimeState = CalculatorState Concrete
type SymbolicState = CalculatorState Symbolic
