module Main where

import CalculatorAnalyzer
import CalculatorDSL
import CalculatorParser
import System.IO (isEOF)
import Text.Megaparsec (errorBundlePretty)
import qualified Data.Text as T

-- Read a program to EOF
readProgramToEOF :: IO (Either String Program)
readProgramToEOF = readStatement 1 []
    where
        readStatement :: Int -> [Statement] -> IO (Either String Program)
        readStatement lineNum acc = do
            eof <- isEOF
            if eof
            then pure $ Right (Program (reverse acc))
            else do
                line <- getLine
                -- Filter out empty and comment lines.
                case filter (/= ' ') line of
                    ""          -> readStatement (lineNum + 1) acc
                    ('/':'/':_) -> readStatement (lineNum + 1) acc
                    _           -> do
                        let p =
                                parseStatement ("line" ++ show lineNum)
                                    $ T.pack line
                        case p of
                            Left errBundle ->
                                pure $ Left (errorBundlePretty errBundle)
                            Right stmt ->
                                readStatement (lineNum + 1) (stmt : acc)

-- main
main :: IO ()
main = do
    program <- readProgramToEOF
    case program of
        Left parseError ->
            putStrLn $ "\nerror: " ++ parseError
        Right prog -> do
            result <- verifyProgram prog $ env $ programInitialState prog
            case result of
                Left err -> do
                    putStrLn $ "\nerror: " ++ err
                Right model -> do
                    case model of
                        Nothing -> do putStrLn "UNSAT"
                        Just _ -> do putStrLn "SAT"
