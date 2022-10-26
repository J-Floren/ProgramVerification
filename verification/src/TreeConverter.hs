module TreeConverter where

import GCLParser.GCLDatatype

data Tree = 
    Q Stmt |
    Linear Stmt Tree | 
    Branch Expr Tree Tree |
    End
    deriving (Show)

stmtLoop :: Stmt -> Int -> Tree
stmtLoop s@(Assert expr) _ = Q s
stmtLoop (Seq s1 s2) k = treeConverter (s1) (stmtLoop s2 (k-1)) k

treeConverter :: Stmt -> Tree -> Int -> Tree
treeConverter _ _ 0 = End
treeConverter (Block _ s) q k = treeConverter s q (k-1)
treeConverter (Seq s1 s2) q k = treeConverter s1 (treeConverter s2 q (k-1)) (k-1)
treeConverter s@(AAssign _ _ _) q k = Linear s q 
treeConverter s@(Assign _ _) q k = Linear s q
treeConverter (IfThenElse g s1 s2) q k = Branch g (treeConverter s1 q (k-1)) (treeConverter s2 q (k-1))
treeConverter (While g s1) q k = Branch g (treeConverter s1 (treeConverter (While g s1) q (k-1)) (k-1)) q
treeConverter s@(Assume _) q k = Linear s q
treeConverter (Skip) q k = q

