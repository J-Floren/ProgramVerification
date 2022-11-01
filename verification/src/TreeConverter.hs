module TreeConverter where

import Z3.Monad
import GCLParser.GCLDatatype
import Evaluator

data Tree = 
    Q Stmt |
    Local [VarDeclaration] Tree |
    Linear Stmt Tree | 
    Branch Expr Tree Tree |
    End
    deriving (Show)

buildTree :: Stmt -> Int -> Tree
buildTree s@(Assert expr) _ = Q s
buildTree (Seq s1 s2) k = treeConverter (s1) (buildTree s2 (k-1)) k

treeConverter :: Stmt -> Tree -> Int -> Tree
treeConverter _ _ 0 = End
treeConverter (Block def s) q k = Local def (treeConverter s q (k-1))
treeConverter (Seq s1 s2) q k = treeConverter s1 (treeConverter s2 q (k-1)) (k-1)
treeConverter s@(AAssign _ _ _) q k = Linear s q
treeConverter s@(Assign _ _) q k = Linear s q
treeConverter (IfThenElse g s1 s2) q k = Branch g (treeConverter s1 q (k-1)) (treeConverter s2 q (k-1))
treeConverter (While g s1) q k = Branch g (treeConverter s1 (treeConverter (While g s1) q (k-1)) (k-1)) q
treeConverter s@(Assume _) q k = Linear s q
treeConverter (Skip) q _ = q

getLocalDefs :: Tree -> [VarDeclaration]
getLocalDefs (Local def q) = def ++ (getLocalDefs q)
getLocalDefs (Linear _ q) = getLocalDefs q
getLocalDefs (Branch _ q1 q2) = (getLocalDefs q1) ++ (getLocalDefs q2)
getLocalDefs _ = []