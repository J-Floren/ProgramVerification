module Evaluator where

import Z3.Monad
import GCLParser.GCLDatatype

script :: Expr -> [VarDeclaration] -> Bool -> Int -> Z3 (Result, String)
script xpr vars heuristics n = do
    z3_expression <- makeZ3Formula xpr (createZ3Vars vars) n
    simple_expr <- simplify z3_expression
    f <- mkNot (if heuristics then simple_expr else z3_expression)
    assert f
    (z3_result, z3_maybe_model) <- getModel

    case z3_maybe_model of 
        Nothing -> do
            return (z3_result, "Empty model")
        Just a -> do 
            str <- modelToString a
            return (z3_result, str)

-- Choose to not implement: Fail, Dereference, NewStore, LitNull    
makeZ3Formula :: Expr -> [(String, Z3 AST)] -> Int -> Z3 AST
makeZ3Formula (BinopExpr op e1 e2) vars n = do
    ast1 <- makeZ3Formula e1 vars n   
    ast2 <- makeZ3Formula e2 vars n
    binopToZ3 (op, ast1, ast2)
makeZ3Formula (LitI x) _ _ = mkInteger $ toInteger x
makeZ3Formula (LitB x) _ _= mkBool x
makeZ3Formula (Var "N") vars n = mkInteger $ toInteger n
makeZ3Formula (Var x) vars _ = getZ3Var x vars
makeZ3Formula (OpNeg x) vars n = do
    z3x <- makeZ3Formula x vars n
    mkNot z3x
makeZ3Formula (Parens x) vars n = makeZ3Formula x vars n
makeZ3Formula (ArrayElem (Var x) y) vars n = do
    z3Array <- getZ3Var x vars
    z3Index <- makeZ3Formula y vars n
    mkSelect z3Array z3Index
makeZ3Formula (Forall str xpr) vars n = do
    forallSym <- mkStringSymbol ("!" ++ str)
    let quantifier = mkIntVar forallSym
    q <- quantifier
    quantifier' <- toApp q
    let forallVars = ([(str, quantifier)] ++ vars)
    mkForallConst [] [quantifier'] =<< makeZ3Formula xpr forallVars n
makeZ3Formula (Exists str xpr) vars n = do
    existsSym <- mkStringSymbol ("!" ++ str)
    let quantifier = mkIntVar existsSym
    q <- quantifier
    quantifier' <- toApp q
    let existsVars = ([(str, quantifier)] ++ vars)
    mkExistsConst [] [quantifier'] =<< makeZ3Formula xpr existsVars n
makeZ3Formula (Cond g e1 e2) vars n = do
    trueValue <- makeZ3Formula e1 vars n
    falseValue <- makeZ3Formula e2 vars n
    condition <- makeZ3Formula g vars n
    mkIte condition trueValue falseValue
makeZ3Formula (SizeOf (Var v)) vars _ = getZ3Var ("#" ++ v) vars

-- Ref type not implemented?
typeToAst :: (String, Type) -> (String, Z3 AST)
typeToAst (x, (PType PTInt)) = (x, mkIntVar =<< (mkStringSymbol x))
typeToAst (x, (PType PTBool)) = (x, mkBoolVar =<< (mkStringSymbol x))
typeToAst (x, (AType PTInt)) = (x, (mkIntSort >>= (\intSort -> 
    (mkArraySort intSort intSort) >>= (\arraySort -> 
        (mkStringSymbol x) >>= (\z3Name -> 
            mkVar z3Name arraySort)))))
typeToAst (x, (AType PTBool)) = (x, (mkBoolSort >>= (\boolSort -> 
    (mkIntSort >>= (\intSort -> 
        (mkArraySort intSort boolSort) >>= (\arraySort -> 
            (mkStringSymbol x) >>= (\z3Name -> mkVar z3Name arraySort)))))))



createZ3Vars :: [VarDeclaration] -> [(String, Z3 AST)]
createZ3Vars [] = []
createZ3Vars ((VarDeclaration str t@(AType _)):xs) = [typeToAst $ (str, t), typeToAst $ ("#" ++ str, (PType PTInt))] ++ createZ3Vars xs
createZ3Vars ((VarDeclaration str t):xs) = [typeToAst $ (str, t)] ++ createZ3Vars xs

getZ3Var :: String -> [(String, Z3 AST)] -> Z3 AST
getZ3Var var ((str, z3):xs) = if var == str then z3 else (getZ3Var var xs)

-- Should be implement Alias? Whats the purpose of it?
binopToZ3 :: ( BinOp, AST, AST ) -> Z3 AST
binopToZ3 (And, ast1 , ast2) = mkAnd [ast1 , ast2]
binopToZ3 (Or, ast1 , ast2) = mkOr [ast1 , ast2]
binopToZ3 (Equal, ast1 , ast2) = mkEq ast1 ast2
binopToZ3 (LessThan, ast1 , ast2) = mkLt ast1 ast2
binopToZ3 (LessThanEqual, ast1 , ast2) = mkLe ast1 ast2
binopToZ3 (GreaterThan, ast1 , ast2) = mkGt ast1 ast2
binopToZ3 (GreaterThanEqual, ast1 , ast2) = mkGe ast1 ast2
binopToZ3 (Implication, ast1 , ast2) = mkImplies ast1 ast2
binopToZ3 (Minus, ast1 , ast2) = mkSub [ast1, ast2]
binopToZ3 (Plus, ast1 , ast2) = mkAdd [ast1, ast2]
binopToZ3 (Multiply, ast1 , ast2) = mkMul [ast1, ast2]
binopToZ3 (Divide, ast1 , ast2) = mkDiv ast1 ast2
    
callEval :: Expr -> [VarDeclaration] -> Bool -> Int -> IO (Result, String)
callEval xpr vars heuristics n = evalZ3 $ script xpr vars heuristics n

