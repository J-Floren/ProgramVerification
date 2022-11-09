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

evalWlps :: [(Expr, [Stmt])] -> [VarDeclaration] -> Bool -> Int -> IO (Maybe ([Stmt], String, Int))
evalWlps [] _ _ _ = return Nothing
evalWlps ((wlp, stmt) : xs) vars heuristics c = do
  (res, info) <- callEval wlp vars heuristics
  if res == Sat then return (Just (stmt, info, c)) else evalWlps xs vars heuristics (c+1)

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

      printColor CYAN "Output:"
      res <- evalWlps wlps vars heuristics 1
      case res of
        Nothing -> printColor GREEN "VALID PROGRAM"
        Just (path, info, count) -> do
          printColor RED "INVALID PROGRAM"
          printColor DULL ("Failed path (" ++ show count ++ "): " ++ show path)
          printColor DULL info

      end <- getCPUTime
      let diff = fromIntegral (end - start) / (10 ^ 12)
      printf "Computation time: %0.3f sec\n" (diff :: Double)
