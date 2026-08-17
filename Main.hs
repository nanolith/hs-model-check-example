module Main where

import CalculatorDSL
import qualified Data.Map.Strict as Map

-- Program A: Valid algebraic equivalence (y = x + x, assert y == 2 * x)
progValid :: Domain d => Program d
progValid = Program
  [ Assume $ NotNaN $ Variable "x"
  , Set "y" (Add (Variable "x") (Variable "x"))
  , Assert $
        Equal (Variable "y") $
            Multiply (Literal (literalValue 2.0)) (Variable "x")
  ]

-- Program B: Divide-by-zero vulnerability
progDivZero :: Domain d => Program d
progDivZero = Program
  [ Set "denom" (Subtract (Variable "x") (Literal (literalValue 5.0)))
  , Set "res"   (Divide (Literal (literalValue 100.0)) (Variable "denom"))
  ]

-- Program C: Invalid invariant (y = 2 * x, assert y == x + 3.0)
progAssertFail :: Domain d => Program d
progAssertFail = Program
  [ Set "y" (Multiply (Literal (literalValue 2.0)) (Variable "x"))
  , Assert $ Equal (Variable "y") $
                Add (Variable "x") (Literal (literalValue 3.0))
  ]

-- Program D: Solve for x and y in (y = 5 * x + 7; y = 4 * x + 3)
progFindSolution :: Domain d => Program d
progFindSolution = Program
  [ Set "y1" (Add (Multiply (Literal (literalValue 5.0)) (Variable "x"))
                  (Literal (literalValue 7.0)))
  , Set "y2" (Add (Multiply (Literal (literalValue 4.0)) (Variable "x"))
                  (Literal (literalValue 3.0)))
  , Assert $ Equal (Variable "y1") (Variable "y2")
  , Solve
  ]

--------------------------------------------------------------------------------
-- Main Entry Point
--------------------------------------------------------------------------------

main :: IO ()
main = do
    putStrLn "============================================================"
    putStrLn " 1. FAST NATIVE RUNTIME (Concrete Double Execution)"
    putStrLn "============================================================"
    let concreteEnv = VarEnv $ Map.fromList [("x", 10.0)]
    case runNative progValid concreteEnv of
        Left err -> putStrLn $ "Runtime error: " ++ err
        Right st -> putStrLn $ "Execution result: " ++ show st

    putStrLn "\n============================================================"
    putStrLn " 2. FORMAL VERIFICATION: Algebraic Equivalence"
    putStrLn " Program: y = x + x; assert(y == 2 * x)"
    putStrLn "============================================================"
    let symX = "x" :: SymDouble
    result1 <- verifyProgram progValid $ VarEnv $ Map.fromList [("x", symX)]
    case result1 of
        Left err -> do putStrLn $ "Runtime error: " ++ err
        Right model -> do
            case model of
                Nothing -> do putStrLn "UNSAT - success (EXPECTED)."
                Just _ -> do putStrLn "SAT - counter-example found (UNEXPECTED!)."

    putStrLn "\n============================================================"
    putStrLn " 3. FORMAL VERIFICATION: Divide-by-Zero Detection"
    putStrLn " Program: denom = x - 5.0; res = 100.0 / denom"
    putStrLn "============================================================"
    result2 <- verifyProgram progDivZero $ VarEnv $ Map.fromList [("x", symX)]
    case result2 of
        Left err -> do putStrLn $ "Runtime error: " ++ err
        Right model -> do
            case model of
                Nothing -> do putStrLn "UNSAT - success (UNEXPECTED!)."
                Just _ -> do putStrLn "SAT - counter-example found (EXPECTED)."

    putStrLn "\n============================================================"
    putStrLn " 4. FORMAL VERIFICATION: Invariant Violation"
    putStrLn " Program: y = 2 * x; assert(y == x + 3.0)"
    putStrLn "============================================================"
    result3 <- verifyProgram progAssertFail $ VarEnv $ Map.fromList [("x", symX)]
    case result3 of
        Left err -> do putStrLn $ "Runtime error: " ++ err
        Right model -> do
            case model of
                Nothing -> do putStrLn "UNSAT - success (UNEXPECTED!)."
                Just _ -> do putStrLn "SAT - counter-example found (EXPECTED)."

    putStrLn "\n============================================================"
    putStrLn " 5. Finding a solution for multiple linear equations."
    putStrLn " Program: y1 = 5 * x + 7; y2 = 4 * x + 3; assert(y1 == y2)"
    putStrLn "============================================================"

    result4 <- verifyProgram progFindSolution $ VarEnv $ Map.fromList [("x", symX)]
    case result4 of
        Left err -> do putStrLn $ "Runtime error: " ++ err
        Right model -> do
            case model of
                Nothing -> do putStrLn "UNSAT - failure (UNEXPECTED!)."
                Just _ -> do putStrLn "SAT - solution found (EXPECTED)."
