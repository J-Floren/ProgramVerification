module Evaluator where

import Z3.Monad
import GCLParser.GCLDatatype

script :: Expr -> Z3 (Result)
script xpr = do
    -- (BinopExpr And (BinopExpr LessThanEqual (Var "x") (LitI 0)) (BinopExpr GreaterThanEqual (Var "x") (LitI 10)))
    -- (BinopExpr Implication (BinopExpr Equal (Var "x") (LitI 5)) (BinopExpr Equal (Var "x") (LitI 10)))
    -- (BinopExpr And (BinopExpr Equal (ArrayElem (Var "a") (LitI 1)) (LitI 0)) (BinopExpr Equal (ArrayElem (Var "a") (LitI 1)) (LitI 10)))
    -- (BinopExpr GreaterThanEqual (ArrayElem (Var "a") (LitI 1)) (LitI 10)
    -- let test = (BinopExpr And (BinopExpr Equal (ArrayElem (Var "a") (LitI 1)) (LitI 10)) (BinopExpr Equal (ArrayElem (Var "a") (LitI 1)) (LitI 10)))
    z3_expression <- makeZ3Formula xpr
    assert z3_expression
    (z3_result, _) <- getModel
    -- z3_string <- solverToString
    return (z3_result)

-- Choose to not implement: Fail, Dereference, NewStore, LitNull
makeZ3Formula :: Expr -> Z3 AST
makeZ3Formula (BinopExpr op e1 e2) = do
    ast1 <- makeZ3Formula e1    
    ast2 <- makeZ3Formula e2
    binopToZ3 (op, ast1, ast2)
makeZ3Formula (LitI x) = mkInteger $ toInteger x
makeZ3Formula (LitB x) = mkBool x
makeZ3Formula (Var x) = do 
    zName <- mkStringSymbol x
    mkIntVar zName
makeZ3Formula (OpNeg x) = do
    z3x <- makeZ3Formula x
    mkNot z3x
makeZ3Formula (Parens x) = makeZ3Formula x
makeZ3Formula (ArrayElem (Var x) y) = do
    intSort <- mkIntSort
    arraySort <- mkArraySort intSort intSort
    z3Name <- mkStringSymbol x
    z3Array <- mkVar z3Name arraySort
    z3Index <- makeZ3Formula y
    mkSelect z3Array z3Index


-- Should be implement Alias? Whats the purpose of it?
binopToZ3 :: ( BinOp, AST, AST ) -> Z3 AST
binopToZ3 (And, ast1 , ast2) = mkAnd [ast1 , ast2]
binopToZ3 (Or, ast1 , ast2) = mkOr [ast1 , ast2]
binopToZ3 (Equal, ast1 , ast2) = mkEq ast1 ast2
binopToZ3 (LessThan, ast1 , ast2) = mkLt ast1 ast2
binopToZ3 (LessThanEqual, ast1 , ast2) = mkLe ast1 ast2
binopToZ3 (GreaterThan, ast1 , ast2) = mkGt ast1 ast2
binopToZ3 (GreaterThanEqual, ast1 , ast2) = mkGe ast1 ast2
binopToZ3 (Implication, ast1 , ast2) = mkAnd [ast1 , ast2]
binopToZ3 (Minus, ast1 , ast2) = mkSub [ast1, ast2]
binopToZ3 (Plus, ast1 , ast2) = mkAdd [ast1, ast2]
binopToZ3 (Multiply, ast1 , ast2) = mkMul [ast1, ast2]
binopToZ3 (Divide, ast1 , ast2) = mkDiv ast1 ast2
    
callEval :: Expr -> IO (Result)
callEval xpr = evalZ3 $ script xpr 

