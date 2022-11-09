module TreeConverter where

import Z3.Monad
import GCLParser.GCLDatatype
import Evaluator
import Wlp

data Tree = 
    Q Stmt |
    Local [VarDeclaration] Tree |
    Linear Stmt Tree | 
    Branch Expr Tree Tree |
    End
    deriving (Show)

buildTree :: Stmt -> Int -> Tree
buildTree s@(Assert expr) _ = Q s
buildTree (Seq s1 s@(Assert expr)) k = treeConverter s1 (Q s) k
buildTree (Seq s1 s2@(Seq _ _)) k = treeConverter s1 (buildTree s2 (k-1)) k
buildTree (Seq s1 s2@(Block def s)) k = treeConverter s1 (Local def (buildTree s k)) k

treeConverter :: Stmt -> Tree -> Int -> Tree
treeConverter _ _ 0 = End
treeConverter (Block def s) q k = Local def (treeConverter s q (k-1))
treeConverter (Seq s1 s2) q k = treeConverter s1 (treeConverter s2 q (k-1)) (k-1)
treeConverter s@(AAssign {}) q k = Linear s q
treeConverter s@(Assign _ _) q k = Linear s q
treeConverter s@(Assert expr) _ _ = Q s
treeConverter (IfThenElse g s1 s2) q k = Branch g (treeConverter s1 q (k-1)) (treeConverter s2 q (k-1))
treeConverter (While g s1) q k = Branch g (treeConverter s1 (treeConverter (While g s1) q (k-1)) (k-1)) q
treeConverter s@(Assume _) q k = Linear s q
treeConverter (Skip) q _ = q

getLocalDefs :: Tree -> [VarDeclaration]
getLocalDefs (Local def q) = def ++ getLocalDefs q
getLocalDefs (Linear _ q) = getLocalDefs q
getLocalDefs (Branch _ q1 q2) = getLocalDefs q1 ++ getLocalDefs q2
getLocalDefs _ = []

makeBranch :: Tree -> [Stmt] -> [VarDeclaration] -> Bool -> Bool -> IO Tree
makeBranch t p v True True = do return t
makeBranch (Branch _ q1 _) p v True _ = pruneBranches q1 p v 
makeBranch (Branch _ _ q2) p v _ True = pruneBranches q2 p v 

pruneBranches :: Tree -> [Stmt] -> [VarDeclaration] -> IO Tree
pruneBranches t@End _ v = return t 
pruneBranches t@(Q s) p v = return t
pruneBranches (Local def q) p v = do
    t <- pruneBranches q p (def ++ v)
    return (Local def t)
pruneBranches (Linear s q) p v = do
    t <- pruneBranches q (p ++ [s]) v
    return (Linear s t)
pruneBranches t@(Branch expr _ _) p v = do
    leftAlways <- evalExpr (p ++ [Assert expr]) v
    rightAlways <- evalExpr (p ++ [Assert (OpNeg expr)]) v
    makeBranch t p v leftAlways rightAlways
    

getPaths :: Tree -> [[Stmt]]
getPaths End = []
getPaths (Q s) = [[s]]
getPaths (Local def q) = getPaths q
getPaths (Linear s q) = map (s:) (getPaths q)
getPaths (Branch (OpNeg expr) q1 q2) = 
    map (Assume expr:) (getPaths q2) ++
    map (Assume (OpNeg expr):) (getPaths q1)
getPaths (Branch expr q1 q2) = 
    map (Assume (OpNeg expr):) (getPaths q2) ++
    map (Assume expr:) (getPaths q1)
