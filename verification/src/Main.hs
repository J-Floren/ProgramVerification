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

path = "C:/Users/Tristan/Desktop/Gitlab/ProgramVerification/gclparser/examples/test.gcl"
err = "failed"    

getStmt :: Program -> Stmt
getStmt p = stmt p

checkSat :: [(Result, String)] -> [Result]
checkSat (x:xs) = [fst x] ++ checkSat xs
checkSat [] = []

main :: IO ()
main = do
    -- putStrLn "Maximum depth?"
    -- input <- getLine
    -- let k = (read input :: Int)
    p <- parseGCLfile path
    case p of
        Left e -> putStrLn e
        Right pr -> do
            start <- getCPUTime
            -- let stmt = getStmt pr
            -- print stmt
            -- let parseTree = stmtLoop stmt k
            -- print parseTree
            -- let wlps = getWlps parseTree
            -- print wlps

            -- let z3Arr = mapM callEval wlps
            -- a <- z3Arr
            -- print a

            -- Tree
            let parseTree = buildTree (stmt pr) 20

            -- Create z3 vars
            let vars = (getLocalDefs parseTree) ++ (input pr) ++ (output pr)

            -- Wlp
            let paths = getPaths parseTree
            putStr "Creating wlps\n"
            let wlps = (map (wlp) paths)
            putStr "Mapping to Z3\n"
            z3Arr <- mapM (\wlp -> callEval wlp vars) wlps 
            let results = checkSat z3Arr
            print results

            end <- getCPUTime
            let diff = (fromIntegral (end - start)) / (10^12)
            printf "Computation time: %0.3f sec\n" (diff :: Double)
            -- print z3Arr
            -- (res, info) <- callEval (head $ reverse wlps)
            -- print res >> putStr "\n" >> putStr info 


