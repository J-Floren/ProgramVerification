module Wlp where

import Control.Exception (Exception (toException))
import Evaluator (callEval)
import GCLParser.GCLDatatype
import Z3.Base

hasVar :: String -> Expr -> Bool
hasVar str (BinopExpr op e1 e2) = hasVar str e1 || hasVar str e2
hasVar str v@(Var str1) = str == str1
hasVar str (ArrayElem v index) = hasVar str index
hasVar str (LitI i) = False
hasVar str (LitB b) = False
hasVar str LitNull = False
hasVar str (Parens e) = hasVar str e
hasVar str (OpNeg e) = hasVar str e
hasVar str (SizeOf e) = False

insert :: Expr -> String -> Expr -> Expr
insert expr str (BinopExpr op e1 e2) = BinopExpr op (insert expr str e1) (insert expr str e2)
insert expr str v@(Var str1) = if str == str1 then expr else v
insert expr str (ArrayElem v@(Var str1) index) = ArrayElem (if str1 == str then expr else v) (insert expr str index) 
insert expr str (LitI i) = LitI i
insert expr str (LitB b) = LitB b
insert expr str LitNull = LitNull
insert expr str (Parens e) = Parens (insert expr str e)
insert expr str (Cond g e1 e2) = Cond (insert expr str g) (insert expr str e1) (insert expr str e2)
insert expr str (OpNeg e) = OpNeg (insert expr str e)
insert expr str (SizeOf (Var v)) = if str == "#" ++ v then expr else SizeOf (Var v)
insert expr str (Forall var e) = if str == var then Forall ("!" ++ var) (insert expr str (insert (Var ("!" ++ var)) var e)) else Forall var (insert expr str e)
insert expr str (Exists var e) = if str == var then Exists ("!" ++ var) (insert expr str (insert (Var ("!" ++ var)) var e)) else Exists var (insert expr str e)

ainsert :: Expr -> String -> Expr -> Expr -> Expr
ainsert expr str i (BinopExpr op e1 e2) = BinopExpr op (ainsert expr str i e1) (ainsert expr str i e2)
ainsert expr str i v@(Var str1) = v
ainsert expr str i a@(ArrayElem c@(Var v) index) = if v == str then Cond (opEqual i index) expr a else a
ainsert expr str i (LitI x) = LitI x
ainsert expr str i (LitB x) = LitB x
ainsert expr str i LitNull = LitNull
ainsert expr str i (Parens e) = Parens (ainsert expr str i e)
ainsert expr str i (Cond g e1 e2) = Cond (ainsert expr str i g) (ainsert expr str i e1) (ainsert expr str i e2)
ainsert expr str i (OpNeg e) = OpNeg (ainsert expr str i e)
ainsert expr str i (SizeOf (Var v)) = SizeOf (Var v)
ainsert expr str i (Forall var e) = if hasVar var i then Forall ("!" ++ var) (ainsert expr str i (insert (Var ("!" ++ var)) var e)) else Forall var (ainsert expr str i e)
ainsert expr str i (Exists var e) = if hasVar var i then Exists ("!" ++ var) (ainsert expr str i (insert (Var ("!" ++ var)) var e)) else Exists var (ainsert expr str i e)
ainsert _ _ _ expr = error (show expr)

wlp :: [Stmt] -> Expr
wlp [Assert expr] = expr
wlp ((Assert expr) : xs) = opAnd expr (wlp xs)
wlp ((Assume expr) : xs) = opImplication expr (wlp xs)
wlp ((Assign str expr) : xs) = insert expr str (wlp xs)
wlp ((AAssign str index expr) : xs) = ainsert expr str index (wlp xs)

wlp2 :: [Stmt] -> Expr
wlp2 [Assert expr] = expr
wlp2 ((Assert expr) : xs) = wlp xs
wlp2 ((Assume expr) : xs) = opAnd expr (wlp xs)
wlp2 ((Assign str expr) : xs) = insert expr str (wlp xs)
wlp2 ((AAssign str index expr) : xs) = ainsert expr str index (wlp xs)

evalExpr :: [Stmt] -> [VarDeclaration] -> Int -> IO Bool
evalExpr s v n = do
  let expr = wlp2 s
  (res, info) <- callEval (OpNeg expr) v True n
  case res of
    Sat -> return True
    Unsat -> return False


exprAtoms :: Expr -> Int
exprAtoms (BinopExpr Implication e1 e2) = exprAtoms e1 + exprAtoms e2
exprAtoms (BinopExpr And e1 e2) = exprAtoms e1 + exprAtoms e2
exprAtoms (BinopExpr Or e1 e2) = exprAtoms e1 + exprAtoms e2
exprAtoms (BinopExpr _ e1 e2) = 1
exprAtoms (Parens e) = exprAtoms e
exprAtoms (OpNeg e) = exprAtoms e
exprAtoms (Cond cond e1 e2) = exprAtoms e2
exprAtoms LitB {} = 1
exprAtoms _ = 0

