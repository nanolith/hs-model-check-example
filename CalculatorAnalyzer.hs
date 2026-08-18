module CalculatorAnalyzer where

import CalculatorDSL
import Data.String (fromString)
import Grisette (ssym)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

-- Collect the set of free variables in an expression.
freeVarsEx :: Expression d -> Set.Set String
freeVarsEx (Literal _) = Set.empty
freeVarsEx (Variable v) = Set.singleton v
freeVarsEx (Add e1 e2) = freeVarsEx e1 `Set.union` freeVarsEx e2
freeVarsEx (Subtract e1 e2) = freeVarsEx e1 `Set.union` freeVarsEx e2
freeVarsEx (Multiply e1 e2) = freeVarsEx e1 `Set.union` freeVarsEx e2
freeVarsEx (Divide e1 e2) = freeVarsEx e1 `Set.union` freeVarsEx e2
freeVarsEx (Negate e1) = freeVarsEx e1

-- Collect the set of free variables in a relational expression
freeVarsRel :: RelationalExpression d -> Set.Set String
freeVarsRel (Equal e1 e2) = freeVarsEx e1 `Set.union` freeVarsEx e2
freeVarsRel (NotEqual e1 e2) = freeVarsEx e1 `Set.union` freeVarsEx e2
freeVarsRel (NotNaN e1) = freeVarsEx e1
freeVarsRel (LessThan e1 e2) = freeVarsEx e1 `Set.union` freeVarsEx e2
freeVarsRel (LessThanEqual e1 e2) = freeVarsEx e1 `Set.union` freeVarsEx e2
freeVarsRel (GreaterThan e1 e2) = freeVarsEx e1 `Set.union` freeVarsEx e2
freeVarsRel (GreaterThanEqual e1 e2) = freeVarsEx e1 `Set.union` freeVarsEx e2

-- Accumulate free variables and defined variables given a statement
freeVarsAccStmt :: (Set.Set String, Set.Set String) -> Statement d
        -> (Set.Set String, Set.Set String)
freeVarsAccStmt (freeSet, definedSet) (Set var e) =
    let vars    = freeVarsEx e
        newVars = vars Set.\\ definedSet
    in (freeSet `Set.union` newVars, Set.insert var definedSet)
freeVarsAccStmt (freeSet, definedSet) (Assume rel) =
    let vars    = freeVarsRel rel
        newVars = vars Set.\\ definedSet
    in (freeSet `Set.union` newVars, definedSet)
freeVarsAccStmt (freeSet, definedSet) (Assert rel) =
    let vars    = freeVarsRel rel
        newVars = vars Set.\\ definedSet
    in (freeSet `Set.union` newVars, definedSet)
freeVarsAccStmt (freeSet, definedSet) _ = (freeSet, definedSet)

-- Get the free variables for a program.
freeVarsProgram :: Program d -> [String]
freeVarsProgram (Program stmts) =
    Set.toList $ fst $ foldl freeVarsAccStmt (Set.empty, Set.empty) stmts

-- Create the initial symbolic state for a program
programInitialState :: Program Symbolic -> CalculatorState Symbolic
programInitialState =
    initialState
        . VarEnv
        . Map.fromList
        . map (\x -> (x, ssym (fromString x) :: SymDouble))
        . freeVarsProgram

-- Is this program a compute program?
isProgramCompute :: Program d -> Bool
isProgramCompute (Program (Compute : _)) = True
isProgramCompute (Program (_ : xs)) = isProgramCompute (Program xs)
isProgramCompute _ = False
