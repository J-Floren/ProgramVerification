module Main where

import Data.Either
import GCLParser.Parser
import GCLParser.PrettyPrint
import GCLParser.GCLDatatype

path = "/Users/jessefloren/Documents/PV/ProgramVerification/gclparser/examples/temp.gcl"
err = "failed"

getStmt :: Program -> Stmt
getStmt p = stmt p

-- stmtLoop :: Stmt -> Expr
-- stmtLoop (Assert expr) = expr
-- stmtLoop (Seq s1 s2) = wlp (s1) (stmtLoop s2)

-- wlp :: Stmt -> Expr -> Expr
-- wlp (Assign str expr) expr1 = insert expr str expr1
-- wlp (Assume expr) expr1 = expr1
-- wlp (IfThenElse guard s1 s2) expr = BinopExpr Or (Parens(BinopExpr And (Parens(guard)) (wlp s1 expr))) (Parens(BinopExpr And (OpNeg guard) (wlp s2 expr)))
-- wlp (Skip) expr = expr

-- insert :: Expr -> String -> Expr -> Expr
-- insert expr str (BinopExpr op e1 e2) = BinopExpr op (insert expr str e1) (insert expr str e2)
-- insert expr str (Var str1) = if str == str1 then expr else (Var str1)
-- insert expr str (LitI i) = LitI i
-- insert expr str (Parens e) = Parens (insert expr str e)
-- insert expr str (OpNeg e) = OpNeg (Parens(insert expr str e))

data Tree = 
    Q Stmt |
    Linear Stmt Tree | 
    Branch Expr Tree Tree |
    Loop Stmt Tree
    deriving (Show)

data Path = 
    Node Stmt |
    LoopNode Stmt [[Path]] |
    EndNode Stmt
    deriving (Show)


stmtLoop :: Stmt -> Tree
stmtLoop s@(Assert expr) = Q s
stmtLoop (Seq s1 s2) = treeConverter (s1) (stmtLoop s2)

treeConverter :: Stmt -> Tree -> Tree
treeConverter s@(Assign _ _) q = Linear s q
treeConverter (IfThenElse g s1 s2) q = Branch g (Linear s1 q) (Linear s2 q)
treeConverter (While g s1) q = Branch g (Loop s1 q) q
treeConverter s@(Assume expr) q = Linear s q
treeConverter (Skip) q = q

getPaths :: Tree -> [[Path]]
getPaths (Q s) = [[EndNode s]]
getPaths (Linear s q) = map (\xs -> (Node s):xs) (getPaths q)
getPaths (Branch expr q1 q2) = 
    [y:ys | y <- [(Node (Assume (OpNeg (Parens expr))))], ys <- (getPaths q2) ] ++
    [x:xs | x <- [(Node (Assume expr))], xs <- (getPaths q1) ]
getPaths (Loop s q) = [[LoopNode s (getPaths q)]]

expandLoopPath :: [Path] -> [[Path]]
expandLoopPath [] = [[]]
expandLoopPath (a@(Node (Assume expr)):l@(LoopNode s q):xs) = 
    [y ++ ys | y <- [[(Node s), (Node (Assume (OpNeg (Parens expr))))]], ys <- q ] ++
    [[(Node s), a, l]]
expandLoopPath (x:xs) = map (\ys -> x:ys) (expandLoopPath xs)

expandTillK :: Int -> [[Path]] -> [[Path]]
expandTillK _ [] = [];
expandTillK k (x:xs) | (length ex) < k = expandTillK k (expandTillK k xs ++ ex)
                     | otherwise = expandTillK k xs
                       where
                            ex = expandLoopPath x

main :: IO ()
main = do
    putStrLn "Maximum depth?"
    input <- getLine
    let k = (read input :: Int)
    p <- parseGCLfile path
    case p of
        Left e -> putStrLn e
        Right pr -> do
            print $ expandTillK k (getPaths(stmtLoop (getStmt pr)))