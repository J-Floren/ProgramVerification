module Wlp where

import GCLParser.GCLDatatype
import TreeConverter
import Control.Exception (Exception(toException))

getPaths :: Tree -> [[Stmt]]
getPaths (End) = []
getPaths (Q s) = [[s]]
getPaths (Local def q) = getPaths q
getPaths (Linear s q) = map (s:) (getPaths q)
getPaths (Branch (OpNeg expr) q1 q2) = 
    (map ((Assume expr):) (getPaths q2)) ++
    (map ((Assume (OpNeg expr)):) (getPaths q1))
getPaths (Branch expr q1 q2) = 
    (map ((Assume (OpNeg expr)):) (getPaths q2)) ++
    (map ((Assume expr):) (getPaths q1))

hasVar :: String -> Expr -> Bool
hasVar str (BinopExpr op e1 e2) = (hasVar str e1) || (hasVar str e2)
hasVar str v@(Var str1) = if str == str1 then True else False
hasVar str (ArrayElem v index) = hasVar str index
hasVar str (LitI i) = False
hasVar str (LitB b) = False
hasVar str (LitNull) = False
hasVar str (Parens e) = hasVar str e
hasVar str (OpNeg e) = hasVar str e

insert :: Expr -> String -> Expr -> Expr
insert expr str (BinopExpr op e1 e2) = BinopExpr op (insert expr str e1) (insert expr str e2)
insert expr str v@(Var str1) = if str == str1 then expr else v
insert expr str (ArrayElem v index) = ArrayElem v (insert expr str index)
insert expr str (LitI i) = LitI i
insert expr str (LitB b) = LitB b
insert expr str (LitNull) = LitNull
insert expr str (Parens e) = Parens (insert expr str e)
insert expr str(Cond g e1 e2) = Cond (insert expr str g) (insert expr str e1) (insert expr str e2)
insert expr str (OpNeg e) = OpNeg (insert expr str e)
insert expr str (SizeOf (Var v)) = SizeOf (Var v)
insert expr str (Forall var e) = (Forall var (if var == str then e else (insert expr str e)))
insert expr str (Exists var e) = (Exists var (if var == str then e else (insert expr str e)))

ainsert :: Expr -> String -> Expr -> Expr -> Expr
ainsert expr str i (BinopExpr op e1 e2) = BinopExpr op (ainsert expr str i e1) (ainsert expr str i e2)
ainsert expr str i v@(Var str1) = v
ainsert expr str i a@(ArrayElem (Var v) index) = if v == str then (Cond (opEqual i index) expr a) else a
ainsert expr str i (LitI x) = LitI x
ainsert expr str i (LitB x) = LitB x
ainsert expr str i (LitNull) = LitNull
ainsert expr str i (Parens e) = Parens (ainsert expr str i e)
ainsert expr str i (Cond g e1 e2) = Cond (ainsert expr str i g) (ainsert expr str i e1) (ainsert expr str i e2)
ainsert expr str i (OpNeg e) = OpNeg (ainsert expr str i e)
ainsert expr str i (SizeOf (Var v)) = SizeOf (Var v)
ainsert expr str i (Forall var e) = (Forall var (if (hasVar var i) then expr else (ainsert expr str i e) ))
ainsert expr str i (Exists var e) = (Exists var (if (hasVar var i) then expr else (ainsert expr str i e) ))
ainsert _ _ _ expr = error (show expr)

wlp :: [Stmt] -> Expr
wlp [Assert expr] = expr
wlp ((Assert expr):xs) = opAnd expr (wlp xs)
wlp ((Assume expr):xs) = opImplication expr (wlp xs)
wlp ((Assign str expr):xs) = insert expr str (wlp xs)
wlp ((AAssign str index expr):xs) = ainsert expr str index (wlp xs)