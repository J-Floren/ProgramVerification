module Main where

import Z3.Monad
import Data.Either
import GCLParser.Parser
import GCLParser.GCLDatatype
import TreeConverter 
import Wlp
import Evaluator
import System.CPUTime
import Text.Printf

path x = "C:/Users/Floren/ProgramVerification/gclparser/examples/" ++ x ++ ".gcl"
err = "failed"    

getStmt :: Program -> Stmt
getStmt p = stmt p

checkSat :: [(Result, String)] -> [Result]
checkSat (x:xs) = [fst x] ++ checkSat xs
checkSat [] = []

isValid :: [Result] -> Bool
isValid [] = True
isValid (Sat:xs) = False
isValid (Unsat:xs) = isValid xs


main :: IO ()
main = do
    putStrLn "File Name"
    f <- getLine
    putStrLn "Maximum depth?"
    i <- getLine
    let k = (read i :: Int)
    p <- parseGCLfile $ path f
    case p of
        Left e -> putStrLn e
        Right pr -> do
            start <- getCPUTime

            -- Tree
            let parseTree = buildTree (stmt pr) k

            -- Create z3 vars
            let vars = getLocalDefs parseTree ++ input pr ++ output pr

            -- Wlp
            let paths = getPaths parseTree
            putStr "Creating wlps\n"
            print paths
            let wlps = (map (wlp) paths)
            putStr "Mapping to Z3\n"
            print wlps
            z3Arr <- mapM (\wlp -> callEval wlp vars) wlps 
            let results = checkSat z3Arr
            print (isValid results)

            end <- getCPUTime
            let diff = (fromIntegral (end - start)) / (10^12)
            printf "Computation time: %0.3f sec\n" (diff :: Double)


