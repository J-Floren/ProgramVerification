data Wlp deriving (show) = 
    Q Expr |
    LinearWlp Stmt Wlp | 
    BranchWlp Expr Wlp Wlp |
    LoopWlp Stmt |
    AssumeWlp Expr Wlp


stmtLoop :: Stmt -> Wlp
stmtLoop (Assert expr) = Q expr
stmtLoop (Seq s1 s2) = wlp (s1) (stmtLoop s2)

wlp :: Stmt -> Wlp -> Wlp
wlp s@(Assign _ _) q = LinearWlp s q
wlp (IfThenElse g s1 s2) q = BranchWlp g (LinearWlp s1 q) (LinearWlp s2 q)
wlp (While g s1) q = BranchWlp g (LoopWlp s1) q
wlp (Assume expr) q = AssumeWlp expr q
wlp (Skip) q = q




