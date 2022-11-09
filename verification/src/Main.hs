module Main where

import Data.Either
import Evaluator
import GCLParser.GCLDatatype
import GCLParser.Parser
import System.CPUTime
import Text.Printf
import TreeConverter
import Wlp
import Z3.Monad

path x = "C:/Users/Floren/ProgramVerification/gclparser/examples/" ++ x ++ ".gcl"

err = "failed"

data Color = RED | GREEN | CYAN | DULL

printColor :: Color -> String -> IO ()
printColor RED s = putStrLn $ "\ESC[91m" ++ s ++ "\ESC[0m"
printColor CYAN s = putStrLn $ "\ESC[36m" ++ s ++ "\ESC[0m"
printColor GREEN s = putStrLn $ "\ESC[32m" ++ s ++ "\ESC[0m"
printColor DULL s = putStrLn $ "\ESC[38;5;7m" ++ s ++ "\ESC[0m"

checkSat :: [(Result, String)] -> [Result]
checkSat = foldr (\x -> (++) [fst x]) []

isValid :: [Result] -> Bool
isValid [] = True
isValid (Sat : xs) = False
isValid (Unsat : xs) = isValid xs

data EvalReturn = OK ([Stmt], String, Int, Int) | ERR (Int, Int) 

evalWlps :: [(Expr, [Stmt])] -> [VarDeclaration] -> Bool -> Int -> Int -> IO EvalReturn
evalWlps [] _ _ count atoms = return (ERR (count-1, atoms))
evalWlps ((wlp, stmt) : xs) vars heuristics count atoms = do
  (res, info) <- callEval wlp vars heuristics
  let a = atoms + exprAtoms wlp
  if res == Sat then return (OK (stmt, info, count, a)) else evalWlps xs vars heuristics (count+1) a

main :: IO ()
main = do
  printColor CYAN "File Name:"
  f <- getLine
  printColor CYAN "Maximum depth:"
  i <- getLine
  let k = (read i :: Int)
  printColor CYAN "Heuristics? (y/n):"
  h <- getLine
  let heuristics = "y" == h
  p <- parseGCLfile $ path f
  case p of
    Left e -> putStrLn e
    Right pr -> do
      start <- getCPUTime

      -- Tree
      let parseTree = buildTree (stmt pr) k

      -- Create z3 vars
      let vars = getLocalDefs parseTree ++ input pr ++ output pr

      prunedTree <- pruneBranches parseTree [] vars

      -- Wlp
      let paths = getPaths (if heuristics then prunedTree else parseTree)
      let wlps = map (\x -> (wlp x, x)) paths

      res <- evalWlps wlps vars heuristics 1 0

      printColor CYAN "Output:"
      case res of
        ERR (c, atoms) -> do
          printColor GREEN "VALID PROGRAM"
          putStrLn $ "Total paths: " ++ show (length paths)
          putStrLn $ "Evaluated paths: " ++ show c
          putStrLn $ "Evaluated atoms: " ++ show atoms
        OK (path, info, count, atoms) -> do
          printColor RED "INVALID PROGRAM"
          putStrLn $ "Total paths: " ++ show (length paths)
          putStrLn $ "Evaluated paths: " ++ show count
          putStrLn $ "Evaluated atoms: " ++ show atoms
          putStrLn $ "Failed path: " ++ show path ++ "\n"
          putStrLn "Counter Example: "
          putStrLn info

      end <- getCPUTime
      let diff = fromIntegral (end - start) / (10 ^ 12)

      printf "Computation time: %0.3f sec\n" (diff :: Double)
