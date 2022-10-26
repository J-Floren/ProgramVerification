module Main where

import Data.Either
import GCLParser.Parser
import GCLParser.GCLDatatype
import TreeConverter 
import Wlp
import Evaluator

path = "C:/Users/Tristan/Desktop/Gitlab/ProgramVerification/gclparser/examples/temp.gcl"
err = "failed"    

getStmt :: Program -> Stmt
getStmt p = stmt p

main :: IO ()
main = do
    putStrLn "Maximum depth?"
    input <- getLine
    let k = (read input :: Int)
    p <- parseGCLfile path
    case p of
        Left e -> putStrLn e
        Right pr -> do
            let stmt = getStmt pr
            print stmt
            let parseTree = stmtLoop stmt k
            print parseTree
            let wlps = getWlps parseTree
            print wlps
            let z3Arr = mapM callEval wlps
            a <- z3Arr
            print a

            -- print z3Arr
            -- (res, info) <- callEval (head $ reverse wlps)
            -- print res >> putStr "\n" >> putStr info 


