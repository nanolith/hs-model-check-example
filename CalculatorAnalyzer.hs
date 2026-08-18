module CalculatorAnalyzer where

import CalculatorDSL
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
