module Lang.Simp.IR.MaximalMunch where 


import Control.Monad.State 
import Lang.Simp.Syntax.AST 
import Lang.Simp.IR.PseudoAssembly
import Lang.Simp.IR.Util




class Cogen t where 
    cogen :: t -> State StateInfo [LabeledInstr]

{-
    for i in {1,n}    G(stmt_i) |- instrs_i
-------------------------------------------------------- (Sequence)
    G(stmt_1,...,stmt_n) |- instrs_1 ++ ... ++  instrs_n
-}

instance Cogen t => Cogen [t] where 
    cogen ts = do 
        x <- mapM cogen ts 
        return (concat x)

instance Cogen Stmt where 
    cogen stmt = case stmt of 
{-
        ------------- (Nop)
        G(nop) |- []
-}
        { Nop          -> return []
{-
     G(x)(e) |- instrs
    -------------------------------------------------------- (Assign)
     G(x = e) |- instrs
-}
        ; Assign x exp -> cogenExp (Temp (var2AVar x)) exp 
        ; Ret x -> let r = mkRegstr "_r_ret" 
                   in do 
                    { l   <- cogenExp r (VarExp x)
                    ; lbl <- newLabel
                    ; return (l ++ [(lbl, IRet)]) 
                    }
{-
    t is a fresh var
    G(t)(cond) |- instrs0
    LIfCondJ is a fresh label

    G(thn) |- instrs2
    LEndThen is a fresh label 

    LElse is the next label (w/o incr)

    G(els) |- instrs3
    LEndElse is a fresh label

    LEndIf is the next label (w/o incr)
    instrs1 = [LIf: ifn t goto LElse ]
    instrs2' = instrs2 ++ [LEndThen:goto LEndIf]
    instrs3' = instrs3 ++ [LEndElse:goto LEndIf]
    -------------------------------------------------------- (If)
    G(if cond {thn} else {els}) |- instrs0 ++ instrs1 ++ instrs2' ++ instrs3' 
-}
        ; If cond thn els -> do 
            { t        <- newTemp
            ; instrs0  <- cogenExp t cond 
            ; lblIfCj  <- newLabel
            
            ; instrs2  <- cogen thn
            ; lblEThen <- newLabel 

            ; lblElse  <- newLabel 
            ; instrs3  <- cogen els
            ; lblEElse <- newLabel 

            ; lblEIf   <- chkNextLabel
            ; let instrs1 = [(lblIfCj, IIfNot t lblElse)]
                  instrs2' = instrs2 ++ [(lblEThen, IGoto lblEIf)]
                  instrs3' = instrs3 ++ [(lblEElse, IGoto lblEIf)]  
            ; return $ instrs0 ++ instrs1 ++ instrs2' ++ instrs3'  
            } 
{-
    LBWhile is the next label (w/o incr)
    t is a fresh var    
    G(t)(cond) |- instrs0
    LWhileCj is a fresh label
    G(body) |- instrs2 
    LEndBody is a fresh label 

    LEndWhile is the next label (w/o incr)

    instrs1 = [LWhile: ifn t goto LEndWhile]
    instrs2' = instrs2 ++ [ LEndBody: goto LBWhile ]
    ----------------------------------------------------- (While)
    G(while cond {body}) |- instrs0 ++ instrs1 ++ insts2'
-}
        ; While cond body -> do 
            { lblBWhile   <- chkNextLabel
            ; t           <- newTemp
            ; instrs0     <- cogenExp t cond

            ; lblWhileCj  <- newLabel 
            ; instrs2     <- cogen body 
            ; lblEndBody  <- newLabel

            ; lblEndWhile <- chkNextLabel 

            ; let instrs1     =  [(lblWhileCj, IIfNot t lblEndWhile)]
                  instrs2'    =  instrs2++[(lblEndBody, IGoto lblBWhile)]
            ; return $ instrs0 ++ instrs1 ++ instrs2'
            }
        }




-- | the function `cogenExp` generates labeled instructions from a regster and an expr
cogenExp :: Opr -> Exp -> State StateInfo [LabeledInstr]
{-
        L is a fresh label
        ---------------------- (Const)
        G(X)(c) |- [L: X <- c]  
-}
cogenExp x (ConstExp (IntConst v)) = do 
    { lbl <- newLabel
    ; return [(lbl,IMove x (IntLit v))] 
    }
cogenExp x (ConstExp (BoolConst True)) = do 
    { lbl <- newLabel
    ; return [(lbl,IMove x (IntLit 1))] 
    }
cogenExp x (ConstExp (BoolConst False)) = do 
    { lbl <- newLabel
    ; return [(lbl,IMove x (IntLit 0))] 
    }
{-
        L is a fresh label
        ---------------------- (Var)
        G(X)(Y) |- [L: X <- Y]   
-}
cogenExp x (VarExp v) = do 
    { lbl <- newLabel
    ; return [(lbl, IMove x (Temp (var2AVar v)))]
    }
{-
        G(X)(E) |- instrs
        ---------------------- (Paren)
        G(X)((E)) |- instrs
-}
cogenExp x (ParenExp e) = cogenExp x e 
{-
        t1 is a fresh var    t2 is a fresh var
        G(t1)(e1) |- instrs1 
        G(t2)(e2) |- instrs2 
        L is a fresh label
        ------------------------------------------ (Op)
        G(X)(e1 op e2) |- instrs1 ++ instrs2 ++ [ L:X <- t1 op t2]
-}
cogenExp x (Minus e1 e2) = do
    { t1  <- newTemp
    ; t2  <- newTemp
    ; l1  <- cogenExp t1 e1
    ; l2  <- cogenExp t2 e2
    ; lbl <- newLabel
    ; return $ l1++l2++[(lbl, IMinus x t1 t2)]
    }
cogenExp x (Plus e1 e2) = do
    { t1  <- newTemp
    ; t2  <- newTemp
    ; l1  <- cogenExp t1 e1
    ; l2  <- cogenExp t2 e2
    ; lbl <- newLabel
    ; return $ l1++l2++[(lbl, IPlus x t1 t2)]
    }
cogenExp x (Mult e1 e2) = do
    { t1  <- newTemp
    ; t2  <- newTemp
    ; l1  <- cogenExp t1 e1
    ; l2  <- cogenExp t2 e2
    ; lbl <- newLabel
    ; return $ l1++l2++[(lbl, IMult x t1 t2)]
    }
cogenExp x (DEqual e1 e2) = do
    { t1  <- newTemp
    ; t2  <- newTemp
    ; l1  <- cogenExp t1 e1
    ; l2  <- cogenExp t2 e2
    ; lbl <- newLabel
    ; return $ l1++l2++[(lbl, IDEqual x t1 t2)]
    }
cogenExp x (LThan e1 e2) = do
    { t1  <- newTemp
    ; t2  <- newTemp
    ; l1  <- cogenExp t1 e1
    ; l2  <- cogenExp t2 e2
    ; lbl <- newLabel
    ; return $ l1++l2++[(lbl, ILThan x t1 t2)]
    }




