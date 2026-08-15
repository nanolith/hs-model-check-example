module CalculatorDSL where

import GHC.Generics (Generic)
import Grisette (
    (.==), (./=), (.<), (.<=), (.>), (.>=), (.&&), Default(..), EvalSym(..),
    Mergeable, Model, FPRoundingMode(RNE), SymBool, SymFP64, fpAdd, fpDiv,
    fpMul, fpSub, rootStrategy, solve, symFpIsNaN, symNot, toSym, wrapStrategy,
    z3)
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
    | Subtract (Expression d) (Expression d)
    | Multiply (Expression d) (Expression d)
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
    | Assume (RelationalExpression d)
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
    , assumptions :: Cond d
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

-- Computational domain
class (Show (Val d), Show (Cond d), Eq (Val d), Eq (Cond d)) => Domain d where
    literalValue            :: Double -> Val d
    addValue                :: Val d -> Val d -> Val d
    subtractValue           :: Val d -> Val d -> Val d
    multiplyValue           :: Val d -> Val d -> Val d
    divideValue             :: Val d -> Val d -> Val d
    equalValue              :: Val d -> Val d -> Cond d
    notEqualValue           :: Val d -> Val d -> Cond d
    lessThanValue           :: Val d -> Val d -> Cond d
    lessThanEqualValue      :: Val d -> Val d -> Cond d
    greaterThanValue        :: Val d -> Val d -> Cond d
    greaterThanEqualValue   :: Val d -> Val d -> Cond d
    isNotNaN                :: Val d -> Cond d
    notEqualZero            :: Val d -> Cond d
    andConditional          :: Cond d -> Cond d -> Cond d
    trueConditional         :: Cond d

-- Concrete domain used for running programs.
instance Domain Concrete where
    literalValue            = id
    addValue                = (+)
    subtractValue           = (-)
    multiplyValue           = (*)
    divideValue             = (/)
    equalValue              = (==)
    notEqualValue           = (/=)
    lessThanValue           = (<)
    lessThanEqualValue      = (<=)
    greaterThanValue        = (>)
    greaterThanEqualValue   = (>=)
    isNotNaN v              = not (isNaN v)
    notEqualZero            = (/= 0)
    andConditional          = (&&)
    trueConditional         = True

-- Symbolic domain used for model checking program
instance Domain Symbolic where
    literalValue            = toSym
    addValue                = fpAdd $ toSym RNE
    subtractValue           = fpSub $ toSym RNE
    multiplyValue           = fpMul $ toSym RNE
    divideValue             = fpDiv $ toSym RNE
    equalValue              = (.==)
    notEqualValue           = (./=)
    lessThanValue           = (.<)
    lessThanEqualValue      = (.<=)
    greaterThanValue        = (.>)
    greaterThanEqualValue   = (.>=)
    isNotNaN v              = symNot (symFpIsNaN v)
    notEqualZero v          = v ./= toSym (0.0 :: Double)
    andConditional          = (.&&)
    trueConditional         = toSym True

-- Define the initial state for a calculator.
initialState :: Domain d => VarEnv (Val d) -> CalculatorState d
initialState initialEnv = CalculatorState {
    env                   = initialEnv
  , assumptions           = trueConditional
  , assertions            = trueConditional
  , safeDivideConditional = trueConditional
  }

-- Evaluate an expression
evalExpression :: Domain d => Expression d -> CalculatorState d
        -> Either String (Val d, CalculatorState d)
evalExpression expr st = case expr of
  Literal n -> Right (n, st)

  Variable name ->
    case Map.lookup name $ unVarEnv $ env st of
      Just val -> Right (val, st)
      Nothing  -> Left $ "Scope Error: Variable '" ++ name ++ "' not found."

  Add e1 e2 -> do
    (v1, st1) <- evalExpression e1 st
    (v2, st2) <- evalExpression e2 st1
    Right (addValue v1 v2, st2)

  Subtract e1 e2 -> do
    (v1, st1) <- evalExpression e1 st
    (v2, st2) <- evalExpression e2 st1
    Right (subtractValue v1 v2, st2)

  Multiply e1 e2 -> do
    (v1, st1) <- evalExpression e1 st
    (v2, st2) <- evalExpression e2 st1
    Right (multiplyValue v1 v2, st2)

  Divide e1 e2 -> do
    (v1, st1) <- evalExpression e1 st
    (v2, st2) <- evalExpression e2 st1
    let st3 = st2 {
        safeDivideConditional =
            andConditional (safeDivideConditional st2) (notEqualZero v2) }
    Right (divideValue v1 v2, st3)

evalRelationalExpression :: Domain d => RelationalExpression d
        -> CalculatorState d -> Either String (Cond d, CalculatorState d)
evalRelationalExpression stmt st =
    case stmt of
        Equal ex1 ex2 -> do
            (lhs, st1) <- evalExpression ex1 st
            (rhs, st2) <- evalExpression ex2 st1
            Right $ (equalValue lhs rhs, st2)

        NotEqual ex1 ex2 -> do
            (lhs, st1) <- evalExpression ex1 st
            (rhs, st2) <- evalExpression ex2 st1
            Right $ (notEqualValue lhs rhs, st2)

        LessThan ex1 ex2 -> do
            (lhs, st1) <- evalExpression ex1 st
            (rhs, st2) <- evalExpression ex2 st1
            Right $ (lessThanValue lhs rhs, st2)

        LessThanEqual ex1 ex2 -> do
            (lhs, st1) <- evalExpression ex1 st
            (rhs, st2) <- evalExpression ex2 st1
            Right $ (lessThanEqualValue lhs rhs, st2)

        GreaterThan ex1 ex2 -> do
            (lhs, st1) <- evalExpression ex1 st
            (rhs, st2) <- evalExpression ex2 st1
            Right $ (greaterThanValue lhs rhs, st2)

        GreaterThanEqual ex1 ex2 -> do
            (lhs, st1) <- evalExpression ex1 st
            (rhs, st2) <- evalExpression ex2 st1
            Right $ (greaterThanEqualValue lhs rhs, st2)

-- Evaluate a statement
evalStatement :: Domain d => Statement d -> CalculatorState d
        -> Either String (CalculatorState d)
evalStatement stmt st =
    case stmt of
        Set name expr -> do
            (val, st') <- evalExpression expr st
            Right $ st' { env =
                            VarEnv $ Map.insert name val $ unVarEnv $ env st' }

        Unset name ->
            if Map.member name $ unVarEnv $ env st
                then Right $ st { env =
                                    VarEnv $ Map.delete name
                                        $ unVarEnv $ env st }
                else Left
                        $ "Scope Error: Cannot delete nonexistent variable '"
                                ++ name ++ "'."

        Assume rel -> do
            (assumption, st') <- evalRelationalExpression rel st
            Right $ st' { assumptions =
                                andConditional (assumptions st') assumption }

        Assert rel -> do
            (assertion, st') <- evalRelationalExpression rel st
            Right $ st' { assertions =
                                andConditional (assertions st') assertion }

-- Run a calculator program
evalProgram :: Domain d => Program d -> CalculatorState d
        -> Either String (CalculatorState d)
evalProgram (Program []) st = Right st
evalProgram (Program (s:ss)) st = do
    st' <- evalStatement s st
    evalProgram (Program ss) st'

-- Run the native program
runNative :: Program Concrete -> VarEnv (Val Concrete)
        -> Either String (CalculatorState Concrete)
runNative prog initialEnv = evalProgram prog (initialState initialEnv)

-- Verify a program
verifyProgram :: Program Symbolic -> VarEnv (Val Symbolic)
        -> IO (Either String (Maybe Model))
verifyProgram prog initialEnv = do
    case evalProgram prog (initialState initialEnv) of
        Left err -> pure $ Left err
        Right finalState -> do
            -- Contract holds if assertions hold AND all divisions were non-zero
            let totalContract =
                    assertions finalState .&& safeDivideConditional finalState
            let violation = symNot totalContract

            solverResult <- solve z3 violation
            case solverResult of
                Left _      -> pure $ Right Nothing -- UNSAT: success
                Right model -> pure $ Right $ Just model -- SAT: Counterexample
