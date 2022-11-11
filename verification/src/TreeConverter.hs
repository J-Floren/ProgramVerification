module TreeConverter where

import Evaluator
import GCLParser.GCLDatatype
import Wlp
import Z3.Monad

data Tree
  = Q Stmt
  | Local [VarDeclaration] Tree
  | Linear Stmt Tree
  | Branch Expr Tree Tree
  | End
  deriving (Show)

buildTree :: Stmt -> Int -> Tree
buildTree s@(Assert expr) _ = Q s
buildTree (Seq s1 s@(Assert expr)) k = treeConverter s1 (Q s) k
buildTree (Seq s1 s2@(Seq _ _)) k = treeConverter s1 (buildTree s2 (k - 1)) k
buildTree (Seq s1 s2@(Block def s)) k = treeConverter s1 (Local def (buildTree s k)) k

treeConverter :: Stmt -> Tree -> Int -> Tree
treeConverter _ _ 0 = End
treeConverter (Block def s) q k = Local def (treeConverter s q (k - 1))
treeConverter (Seq s1 s2) q k = treeConverter s1 (treeConverter s2 q (k - 1)) (k - 1)
treeConverter s@(AAssign {}) q k = Linear s q
treeConverter s@(Assign _ _) q k = Linear s q
treeConverter s@(Assert expr) _ _ = Q s
treeConverter (IfThenElse g s1 s2) q k = Branch g (treeConverter s1 q (k - 1)) (treeConverter s2 q (k - 1))
treeConverter (While g s1) q k = Branch g (treeConverter s1 (treeConverter (While g s1) q (k - 1)) (k - 1)) q
treeConverter s@(Assume _) q k = Linear s q
treeConverter Skip q _ = q

getLocalVars :: Stmt -> [VarDeclaration]
getLocalVars (Block def s1) = def ++ getLocalVars s1
getLocalVars (IfThenElse _ s1 s2) = getLocalVars s1 ++ getLocalVars s2
getLocalVars (While _ s1 ) = getLocalVars s1
getLocalVars (Seq s1 s2) = getLocalVars s1 ++ getLocalVars s2
getLocalVars _ = []

makeBranch :: Tree -> [Stmt] -> [VarDeclaration] -> Bool -> Bool -> IO Tree
makeBranch (Branch g q1 q2) p v True True = do 
  x1 <- pruneBranches q1 p v
  x2 <- pruneBranches q2 p v
  return (Branch g x1 x2)
makeBranch (Branch _ q1 _) p v True _ = do pruneBranches q1 p v
makeBranch (Branch _ _ q2) p v _ True = do pruneBranches q2 p v

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

isArrayType :: Type -> Bool
isArrayType (AType _) = True
isArrayType _ = False

isArray :: [VarDeclaration] -> String -> Bool
isArray [] _ = False
isArray ((VarDeclaration str t):xs) var = if var == str then isArrayType t else isArray xs var

getPaths :: Tree -> [VarDeclaration] -> [[Stmt]]
getPaths End vars = []
getPaths (Q s) vars = [[s]]
getPaths (Local def q) vars = getPaths q vars
getPaths (Linear s@(Assign a (Var b)) q) vars = if isArray vars a then map ([s,Assign ("#" ++ a) (Var ("#" ++ b))]++) (getPaths q vars) else map (s :) (getPaths q vars)
getPaths (Linear s q) vars = map (s :) (getPaths q vars)
getPaths (Branch (OpNeg expr) q1 q2) vars =
  map (Assume expr :) (getPaths q2 vars)
    ++ map (Assume (OpNeg expr) :) (getPaths q1 vars)
getPaths (Branch expr q1 q2) vars =
  map (Assume (OpNeg expr) :) (getPaths q2 vars)
    ++ map (Assume expr :) (getPaths q1 vars)
