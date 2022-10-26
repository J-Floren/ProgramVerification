module Wlp where

import GCLParser.GCLDatatype
import TreeConverter

wlp :: Stmt -> Expr -> Expr
wlp (Assign str expr) expr1 = insert expr str expr1
wlp (Assume expr) expr1 = opImplication expr expr1
wlp (Skip) expr = expr

insert :: Expr -> String -> Expr -> Expr
insert expr str (BinopExpr op e1 e2) = BinopExpr op (insert expr str e1) (insert expr str e2)
insert expr str v@(Var str1) = if str == str1 then expr else v
insert expr str (ArrayElem v index) = ArrayElem v (insert expr str index)
insert expr str (LitI i) = LitI i
insert expr str (LitB b) = LitB b
insert expr str (LitNull) = LitNull
insert expr str (Parens e) = Parens (insert expr str e)
insert expr str (OpNeg e) = OpNeg (insert expr str e)

getWlps :: Tree -> [Expr]
getWlps (End) = []
getWlps (Q (Assert expr)) = [expr]
getWlps (Linear s q) = map (wlp s) (getWlps q)
getWlps (Branch (OpNeg expr) q1 q2) = 
    (map (opImplication expr) (getWlps q2)) ++
    (map (opImplication (OpNeg expr)) (getWlps q1))
getWlps (Branch expr q1 q2) = 
    (map (opImplication (OpNeg expr)) (getWlps q2)) ++
    (map (opImplication expr) (getWlps q1))