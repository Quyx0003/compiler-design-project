module Lang.Simp.IR.MaximalMunchSpec where 

import Prelude hiding (lex)
import Test.Hspec
import Control.Monad.State
import Lang.Simp.Syntax.AST
import Lang.Simp.IR.PseudoAssembly
import Lang.Simp.IR.MaximalMunch 
import Lang.Simp.IR.Util



spec :: Spec
spec = do
    describe "maximal_munch" $ do

        it "test maximal munch: y = 1; x = y + 1" $ 
            let st = StateInfo 1 "var" 1
                stmt1 = Assign (Var "y") (ConstExp (IntConst 1))
                stmt2 = Assign (Var "x") (Plus (VarExp (Var "y")) (ConstExp (IntConst 1)))
                input = [stmt1, stmt2]
                expected = [
                    (1, IMove (Temp (AVar "y")) (IntLit 1)), 
                    (2, IMove (Temp (AVar "var_1")) (Temp (AVar "y"))), 
                    (3, IMove (Temp (AVar "var_2")) (IntLit 1)), 
                    (4, IPlus (Temp (AVar "x")) (Temp (AVar "var_1")) (Temp (AVar "var_2")))]
            in case runState (cogen input) st of 
                { (instrs, st) -> instrs `shouldBe` expected
                } 

        it "test maximal munch: z = (x + 3) * (y - 5); return z" $ 
            let st = StateInfo 1 "var" 1
                stmt3 = Assign (Var "z") (Mult (Plus (VarExp (Var "x")) (ConstExp (IntConst 3))) (Minus (VarExp (Var "y")) (ConstExp (IntConst 5))))
                stmt4 = Ret (Var "z")          
                input = [stmt3, stmt4]
                expected = [
                    (1, IMove (Temp (AVar "var_3")) (Temp (AVar "x"))), 
                    (2, IMove (Temp (AVar "var_4")) (IntLit 3)), 
                    (3, IPlus (Temp (AVar "var_1")) (Temp (AVar "var_3")) (Temp (AVar "var_4"))), 
                    (4, IMove (Temp (AVar "var_5")) (Temp (AVar "y"))), 
                    (5, IMove (Temp (AVar "var_6")) (IntLit 5)), 
                    (6, IMinus (Temp (AVar "var_2")) (Temp (AVar "var_5")) (Temp (AVar "var_6"))), 
                    (7, IMult (Temp (AVar "z")) (Temp (AVar "var_1")) (Temp (AVar "var_2"))), 
                    (8, IMove (Regstr "_r_ret") (Temp (AVar "z"))), 
                    (9, IRet)]
            in case runState (cogen input) st of 
                { (instrs, st) -> instrs `shouldBe` expected
                } 

        it ("test maximal munch: " ++ "x = 0\n\
        \y = 10\n\
        \i = 0\n\
        \while (i < y) {\n\
        \   x = x + i\n\
        \   i = i + 1\n\
        \}\n\
        \return x") $ 
            let st = StateInfo 1 "var" 1
                stmt1 = Assign (Var "x") (ConstExp (IntConst 0))
                stmt2 = Assign (Var "y") (ConstExp (IntConst 10))
                stmt3 = Assign (Var "i") (ConstExp (IntConst 0))
                stmt4 = While (LThan (VarExp (Var "i")) (VarExp (Var "y"))) [
                    Assign (Var "x") (Plus (VarExp (Var "x")) (VarExp (Var "i"))),
                    Assign (Var "i") (Plus (VarExp (Var "i")) (ConstExp (IntConst 1))) ]
                stmt5 = Ret (Var "x")
                input = [stmt1, stmt2, stmt3, stmt4, stmt5]
                expected = [
                    (1, IMove (Temp (AVar "x")) (IntLit 0)), 
                    (2, IMove (Temp (AVar "y")) (IntLit 10)), 
                    (3, IMove (Temp (AVar "i")) (IntLit 0)), 
                    (4, IMove (Temp (AVar "var_2")) (Temp (AVar "i"))), 
                    (5, IMove (Temp (AVar "var_3")) (Temp (AVar "y"))), 
                    (6, ILThan (Temp (AVar "var_1")) (Temp (AVar "var_2")) (Temp (AVar "var_3"))), 
                    (7, IIfNot (Temp (AVar "var_1")) 15), 
                    (8, IMove (Temp (AVar "var_4")) (Temp (AVar "x"))), 
                    (9, IMove (Temp (AVar "var_5")) (Temp (AVar "i"))), 
                    (10,IPlus (Temp (AVar "x")) (Temp (AVar "var_4")) (Temp (AVar "var_5"))), 
                    (11,IMove (Temp (AVar "var_6")) (Temp (AVar "i"))), 
                    (12,IMove (Temp (AVar "var_7")) (IntLit 1)), 
                    (13,IPlus (Temp (AVar "i")) (Temp (AVar "var_6")) (Temp (AVar "var_7"))), 
                    (14,IGoto 4), 
                    (15,IMove (Regstr "_r_ret") (Temp(AVar "x"))), 
                    (16,IRet)]
            in case runState (cogen input) st of 
                { (instrs, st) -> instrs `shouldBe` expected
                } 
