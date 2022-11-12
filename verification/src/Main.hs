module Main where

import Data.Either
import Evaluator
import GCLParser.GCLDatatype
import GCLParser.Parser
import System.CPUTime
import Text.Printf
import TreeConverter
import Wlp
import Z3.Monad ( Result(Sat, Unsat) )
import Data.List
import GHC.IO.IOMode (IOMode(ReadWriteMode))
import System.IO
import MuGCL (mutateProgram, MutationType)

path x = "C:/Users/Floren/ProgramVerification/gclparser/examples/" ++ x ++ ".gcl"

err = "failed"

data Color = RED | GREEN | CYAN 

printColor :: Color -> String -> IO ()
printColor RED s = putStrLn $ "\ESC[91m" ++ s ++ "\ESC[0m"
printColor CYAN s = putStrLn $ "\ESC[36m" ++ s ++ "\ESC[0m"
printColor GREEN s = putStrLn $ "\ESC[32m" ++ s ++ "\ESC[0m"

checkSat :: [(Result, String)] -> [Result]
checkSat = foldr (\x -> (++) [fst x]) []

isValid :: [Result] -> Bool
isValid [] = True
isValid (Sat : xs) = False
isValid (Unsat : xs) = isValid xs

data EvalReturn = FAIL (Expr, [Stmt], String, Int, Int) | SUCCESS (Int, Int) 

evalWlps :: [(Expr, [Stmt])] -> [VarDeclaration] -> Bool -> Int -> Int -> Int -> IO EvalReturn
evalWlps [] _ _ _ count atoms = return (SUCCESS (count-1, atoms))
evalWlps ((wlp, stmt) : xs) vars heuristics n count atoms = do
  (res, info) <- callEval wlp vars heuristics n
  let a = atoms + exprAtoms wlp
  if res == Sat then return (FAIL (wlp, stmt, info, count, a)) else evalWlps xs vars heuristics n (count+1) a

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
  printColor CYAN "Mutate? (y/n):"
  m <- getLine
  let mutate = "y" == m
  if mutate && "benchmark" `isPrefixOf` f then do
    let range = [2..10]
    let x = map (verifyMutations f k heuristics) range
    let path = "C:/Users/Floren/ProgramVerification/output/" ++ (let repl '/' = '_'; repl x = x in  map repl f) ++ "_" ++ h ++ "mutation.csv"

    outputFile <- openFile path ReadWriteMode
    writeMutations outputFile x
    hClose outputFile
  else if "benchmark" `isPrefixOf` f then do
    let range = [2..10]
    let x = map (verifyProgram f k heuristics) range
    let path = "C:/Users/Floren/ProgramVerification/output/" ++ (let repl '/' = '_'; repl x = x in  map repl f) ++ "_" ++ h ++ ".csv"

    outputFile <- openFile path ReadWriteMode
    writeOutput outputFile x
    hClose outputFile
  else do
    if mutate then putStrLn "Can't mutate non benchmark files" else putStrLn "Start Verification: "
    x <- verifyProgram f k heuristics 0 
    putStrLn "Finished"

roundToStr :: (PrintfArg a, Floating a) => Int -> a -> String
roundToStr = printf "%0.*f"

writeOutput :: Handle -> [IO (Bool, Int, Int, Int, Int, Int, Double)] -> IO()
writeOutput _ [] = putStrLn "Finished"
writeOutput outputFile (input:xs) = do
  (_, n, k, paths, count, atoms, diff) <- input
  hPutStrLn outputFile (
    show n ++ "," ++ 
    show k ++ "," ++ 
    show paths ++ "," ++ 
    show count ++ "," ++ 
    show atoms ++ "," ++ 
    roundToStr 3 diff )
  writeOutput outputFile xs

verifyProgram :: String -> Int -> Bool -> Int -> IO (Bool, Int, Int, Int, Int, Int, Double)
verifyProgram f k heuristics n = do
  p <- parseGCLfile $ path f
  case p of
    Left e -> error "An error has occured"
    Right pr -> verify pr k heuristics n

verify :: Program -> Int -> Bool -> Int -> IO (Bool, Int, Int, Int, Int, Int, Double)
verify pr k heuristics n = do
      start <- getCPUTime

      let localVars = getLocalVars (stmt pr)

      -- Tree
      let parseTree = buildTree (stmt pr) k

      -- Create z3 vars
      let vars = localVars ++ input pr ++ output pr

      prunedTree <- pruneBranches parseTree [] vars n

      -- Wlp
      let paths = getPaths (if heuristics then prunedTree else parseTree) vars
      let wlps = map (\x -> (wlp x, x)) paths

      res <- evalWlps wlps vars heuristics n 1 0

      printColor CYAN "Output:"
      case res of
        SUCCESS (count, atoms) -> do
          printColor GREEN "VALID PROGRAM"
          putStrLn $ "Total paths: " ++ show (length paths)
          putStrLn $ "Evaluated paths: " ++ show count
          putStrLn $ "Evaluated atoms: " ++ show atoms

          end <- getCPUTime
          let diff = fromIntegral (end - start) / (10 ^ 12)
          printf "Computation time: %0.3f sec\n" (diff :: Double)

          return (True, n, k, length paths, count, atoms, diff)
        FAIL (wlp, path, info, count, atoms) -> do
          printColor RED "INVALID PROGRAM"
          putStrLn ("Total paths: " ++ show (length paths))
          putStrLn $ "Evaluated paths: " ++ show count
          putStrLn $ "Evaluated atoms: " ++ show atoms
          putStrLn $ "Failed path: " ++ show path
          putStrLn $ "Failed wlp: " ++ show wlp ++ "\n"
          putStrLn "Counter Example: "
          putStrLn info

          end <- getCPUTime
          let diff = fromIntegral (end - start) / (10 ^ 12)
          printf "Computation time: %0.3f sec\n" (diff :: Double)

          return (False, n, k, length paths, count, atoms, diff)

verifyMutations :: String -> Int -> Bool -> Int -> IO Int
verifyMutations f k heuristics n = do
  p <- parseGCLfile $ path f
  case p of
    Left e -> error "An error has occured"
    Right pr -> do
      let x = mutateProgram pr
      let y = map fst x
      print y
      runMutations x k heuristics n

runMutations :: [(MutationType, Program)] -> Int -> Bool -> Int -> IO Int
runMutations [(t, pr)] k heuristics n = do
  putStrLn ("Mutation: " ++ show t)
  (bool,_,_,_,_,_,_) <- verify pr k heuristics n
  if bool then return 0 else return 1
runMutations ((t, pr):xs) k heuristics n = do
  putStrLn ("Mutation: " ++ show t)
  (bool,_,_,_,_,_,_) <- verify pr k heuristics n
  let val = if bool then 0 else 1
  y <- runMutations xs k heuristics n
  return (val + y)

writeMutations :: Handle -> [IO Int] -> IO ()
writeMutations outputFile [x] = do y <- x; hPutStrLn outputFile (show y ++ "; ")
writeMutations outputFile (x:xs) = do y <- x; hPutStrLn outputFile (show y ++ "; "); writeMutations outputFile xs