module Main where

import Data.Either
import GCLParser.Parser
import GCLParser.PrettyPrint
import GCLParser.GCLDatatype

path = "/Users/jessefloren/Documents/PV/ProgramVerification/gclparser/examples/easy.gcl"
err = "failed"

getStmt :: Program -> Stmt
getStmt p = stmt p
 
getWlp :: Stmt -> (Expr, Expr)
getWlp (Seq (Assume expr) s2) = (expr, stmtLoop s2)
getWlp (Seq s1 s2) = (LitNull, stmtLoop (Seq s1 s2))

stmtLoop :: Stmt -> Expr
stmtLoop (Assert expr) = expr
stmtLoop (Seq s1 s2) = wlp (s1) (stmtLoop s2)

wlp :: Stmt -> Expr -> Expr
wlp (Assign str expr) expr1 = insert expr str expr1
wlp (Assume expr) expr1 = expr1
wlp (IfThenElse guard s1 s2) expr = BinopExpr Or (Parens(BinopExpr And (Parens(guard)) (wlp s1 expr))) (Parens(BinopExpr And (OpNeg guard) (wlp s2 expr)))
wlp (Skip) expr = expr

insert :: Expr -> String -> Expr -> Expr
insert expr str (BinopExpr op e1 e2) = BinopExpr op (insert expr str e1) (insert expr str e2)
insert expr str (Var str1) = if str == str1 then expr else (Var str1)
insert expr str (LitI i) = LitI i
insert expr str (Parens e) = Parens (insert expr str e)
insert expr str (OpNeg e) = OpNeg (Parens(insert expr str e))

main :: IO ()
main = do
    p <- parseGCLfile path
    case p of
        Left e -> putStrLn e
        Right pr -> do
            -- print $ (getStmt pr)
            print $ getWlp (getStmt pr)
