module Main where

import Data.Either
import GCLParser.Parser
import GCLParser.PrettyPrint
import GCLParser.GCLDatatype

path = "/Users/jessefloren/Documents/PV/ProgramVerification/gclparser/examples/E.gcl"
err = "failed"

getStmt :: Program -> Stmt
getStmt p = stmt p

main :: IO ()
main = do
    p <- parseGCLfile path
    case p of
        Left e -> putStrLn e
        Right pr -> print $ getStmt pr
            