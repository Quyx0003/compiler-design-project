module Lang.Simp.IR.MMUpDownSpec where

import Prelude hiding (lex)
import Test.Hspec
import Control.Monad.State
import Lang.Simp.Syntax.AST
import Lang.Simp.IR.PseudoAssembly
import Lang.Simp.IR.MMUpDown
import Lang.Simp.IR.Util



spec :: Spec
spec = do
    describe "mm_updown" $ do

        it "cogenExp: y + 1" $
            let st = StateInfo 1 "var" 1
                exp1 = Plus (VarExp (Var "y")) (ConstExp (IntConst 1))
                expected = [(1, IPlus (Temp (AVar "var_1")) (Temp (AVar "y")) (IntLit 1))]
            in case runState (cogenExp exp1) st of
                { ((opr, instrs), st) -> instrs `shouldBe` expected
                }

        it "cogenExp: (x + 3) * (y - 5)" $
            let st = StateInfo 1 "var" 1
                exp = Mult (Plus (VarExp (Var "x")) (ConstExp (IntConst 3))) (Minus (VarExp (Var "y")) (ConstExp (IntConst 5)))
                input = exp
                expected = [
                    (1, IPlus (Temp (AVar "var_1")) (Temp (AVar "x")) (IntLit 3)),
                    (2, IMinus (Temp (AVar "var_2")) (Temp (AVar "y")) (IntLit 5)),
                    (3, IMult (Temp (AVar "var_3")) (Temp (AVar "var_1")) (Temp (AVar "var_2")))]
            in case runState (cogenExp input) st of
                { ((opr, instrs), st) -> instrs `shouldBe` expected
                }


        it "test maximal munch v2: y = 1; x = y + 1" $
            let st = StateInfo 1 "var" 1
                stmt1 = Assign (Var "y") (ConstExp (IntConst 1))
                stmt2 = Assign (Var "x") (Plus (VarExp (Var "y")) (ConstExp (IntConst 1)))
                input = [stmt1, stmt2]
                expected = [
                    (1, IMove (Temp (AVar "y")) (IntLit 1)), 
                    (2, IPlus (Temp (AVar "var_1")) (Temp (AVar "y")) (IntLit 1)), 
                    (3, IMove (Temp (AVar "x")) (Temp (AVar "var_1")))]
            in case runState (cogen input) st of
                { (instrs, st) -> instrs `shouldBe` expected
                }


        it "test maximal munch v2: z = (x + 3) * (y - 5); return z" $
            let st = StateInfo 1 "var" 1
                stmt1 = Assign (Var "z") (Mult (Plus (VarExp (Var "x")) (ConstExp (IntConst 3)))  (Minus (VarExp (Var "y")) (ConstExp (IntConst 5))))
                stmt2 = Ret (Var "z")          
                input = [stmt1, stmt2]
                expected = [
                    (1, IPlus (Temp (AVar "var_1")) (Temp (AVar "x")) (IntLit 3)), 
                    (2, IMinus (Temp (AVar "var_2")) (Temp (AVar "y")) (IntLit 5)), 
                    (3, IMult (Temp (AVar "var_3")) (Temp (AVar "var_1")) (Temp (AVar "var_2"))), 
                    (4, IMove (Temp (AVar "z")) (Temp (AVar "var_3"))), 
                    (5, IMove (Regstr "_r_ret") (Temp (AVar "z"))), 
                    (6, IRet)]
            in case runState (cogen input) st of
                { (instrs, st) -> instrs `shouldBe` expected
                }
        

        it ("test maximal munch v2: " ++ "x = 0\n\
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
                    (1,IMove (Temp (AVar "x")) (IntLit 0)),
                    (2,IMove (Temp (AVar "y")) (IntLit 10)),
                    (3,IMove (Temp (AVar "i")) (IntLit 0)),
                    (4,ILThan (Temp (AVar "var_1")) (Temp (AVar "i")) (Temp (AVar "y"))),
                    (5,IIfNot (Temp (AVar "var_1")) 11),
                    (6,IPlus (Temp (AVar "var_2")) (Temp (AVar "x")) (Temp (AVar "i"))),
                    (7,IMove (Temp (AVar "x")) (Temp (AVar "var_2"))),
                    (8,IPlus (Temp (AVar "var_3")) (Temp (AVar "i")) (IntLit 1)),
                    (9,IMove (Temp (AVar "i")) (Temp (AVar "var_3"))),
                    (10,IGoto 4),
                    (11,IMove (Regstr "_r_ret") (Temp (AVar "x"))),
                    (12,IRet)]
            in case runState (cogen input) st of
                { (instrs, st) -> instrs `shouldBe` expected
                }
