module CalculatorDSL where

import GHC.Generics (Generic)
import Grisette (
    Default(..), EvalSym(..), Mergeable, SymBool, SymFP64, rootStrategy,
    wrapStrategy)
import qualified Data.Map.Strict as Map

-- Double symbolic type
type SymDouble = SymFP64

-- Data tags for supporting concrete and symbolic interpretation
data Concrete
data Symbolic

type family Val d = r | r -> d where
    Val Concrete = Double
    Val Symbolic = SymFP64

type family Cond d = r | r -> d where
    Cond Concrete = Bool
    Cond Symbolic = SymBool

-- Arithmetic expressions
data Expression d =
      Literal (Val d)
    | Variable String
    | Add (Expression d) (Expression d)
    | Multiplication (Expression d) (Expression d)
    | Divide (Expression d) (Expression d)

deriving instance (Eq (Val d)) => Eq (Expression d)
deriving instance (Show (Val d)) => Show (Expression d)

-- Relational expressions
data RelationalExpression d =
      Equal (Expression d) (Expression d)
    | NotEqual (Expression d) (Expression d)
    | LessThan (Expression d) (Expression d)
    | LessThanEqual (Expression d) (Expression d)
    | GreaterThan (Expression d) (Expression d)
    | GreaterThanEqual (Expression d) (Expression d)

deriving instance (Eq (Expression d)) => Eq (RelationalExpression d)
deriving instance (Show (Expression d)) => Show (RelationalExpression d)

-- Statements
data Statement d =
      Set String (Expression d)
    | Unset String
    | Assert (RelationalExpression d)

deriving instance (Eq (Expression d), Eq (RelationalExpression d)) =>
    Eq (Statement d)
deriving instance (Show (Expression d), Show (RelationalExpression d)) =>
    Show (Statement d)

-- Program
data Program d = Program [Statement d]

deriving instance (Eq (Statement d)) => Eq (Program d)
deriving instance (Show (Statement d)) => Show (Program d)

-- Variable environment
newtype VarEnv v = VarEnv { unVarEnv :: Map.Map String v }
  deriving newtype (Show, Eq)

instance Mergeable v => Mergeable (VarEnv v) where
    rootStrategy =
        wrapStrategy rootStrategy (VarEnv . Map.fromList)
                     (Map.toList .  unVarEnv)

instance EvalSym v => EvalSym (VarEnv v) where
    evalSym model subst (VarEnv m) =
        VarEnv $ Map.fromList (evalSym model subst (Map.toList m))

-- Calculator runtime state
data CalculatorState d = CalculatorState {
      env :: VarEnv (Val d)
    , assertions :: Cond d
    , safeDivideConditional :: Cond d
    } deriving stock (Generic)

deriving instance (Eq (Val d), Eq (Cond d)) =>
    Eq (CalculatorState d)
deriving instance (Show (Val d), Show (Cond d)) =>
    Show (CalculatorState d)
deriving via (Default (CalculatorState Symbolic)) instance
    Mergeable (CalculatorState Symbolic)
deriving via (Default (CalculatorState Symbolic)) instance
    EvalSym (CalculatorState Symbolic)

-- Symbolic and Runtime states
type RuntimeState = CalculatorState Concrete
type SymbolicState = CalculatorState Symbolic

class (Show (Val d), Show (Cond d), Eq (Val d), Eq (Cond d)) => Domain d where
    literalValue    :: Double -> Val d
    addValue        :: Val d -> Val d -> Val d
    multiplyValue   :: Val d -> Val d -> Val d
    divideValue     :: Val d -> Val d -> Val d
    equalValue      :: Val d -> Val d -> Cond d
    negativeZero    :: Val d -> Cond d
    andConditional  :: Cond d -> Cond d -> Cond d
    trueConditional :: Cond d
