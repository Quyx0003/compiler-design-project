module Lang.Simp.Interpreter.SimpIntSpec where 

import Test.Hspec
import qualified Data.Map as DM
import Lang.Simp.Syntax.AST
import Lang.Simp.Interpreter.SimpInt 

spec :: Spec
spec = do
    describe "SimpInt" $ do


        it "evalExp: c + s" $
            let s = Var "s"
                c = Var "c" 
                delta = DM.fromList [(s, IntConst 3), (c, IntConst 1)]
                exp1 = Plus (VarExp c) (VarExp s)
                expected = Right (IntConst 4)
            in evalExp delta exp1 `shouldBe` expected

        it "evalExp: c < (s * 2)" $
            let s = Var "s"
                c = Var "c" 
                delta = DM.fromList [(s, IntConst 3), (c, IntConst 1)]
                exp1 = LThan (VarExp c) (Mult (VarExp s) (ConstExp (IntConst 2)))
                expected = Right (BoolConst True)
            in evalExp delta exp1 `shouldBe` expected


        {-
        x = input;
        f = 0;
        s = 1;
        c = 0;
        t = 0;
        while c < x {
            t = f;
            f = s;
            s = t + f;
            c = c + 1;
        }
        return s;
        -}

        it "SimpInt: fib(4)" $ 
            let input = Var "input"
                x     = Var "x"
                f     = Var "f"
                s     = Var "s"
                c     = Var "c"
                t     = Var "t"
                stmts = [
                    Assign x (VarExp input),
                    Assign f (ConstExp (IntConst 0)),
                    Assign s (ConstExp (IntConst 1)),
                    Assign c (ConstExp (IntConst 0)),
                    Assign t (ConstExp (IntConst 0)),
                    While (LThan (VarExp c) (VarExp x)) [
                        Assign t (VarExp f),
                        Assign f (VarExp s),
                        Assign s (Plus (VarExp t) (VarExp f)),
                        Assign c (Plus (VarExp c) (ConstExp (IntConst 1)))
                    ],
                    Ret(s)]
                expected = Right (IntConst 5)
                result = interpret stmts 4
            in result `shouldBe` expected

        {-
            x = input;
            s = 0;
            c = 0;
            while c < x {
              s = c + s;
              c = c + 1;
              }
            return s;"
        -} 
        it "SimpInt: sum(3)" $
            let stmts = [
                    Assign (Var "x") (VarExp (Var "input")), 
                    Assign (Var "s") (ConstExp (IntConst 0)), 
                    Assign (Var "c") (ConstExp (IntConst 0)), 
                    While (LThan (VarExp (Var "c")) (VarExp (Var "x"))) [
                        Assign (Var "s") (Plus (VarExp (Var "c")) (VarExp (Var "s"))), 
                        Assign (Var "c") (Plus (VarExp (Var "c")) (ConstExp (IntConst 1)))
                    ],
                    Ret (Var "s")]
                expected = Right (IntConst 3)
                result = interpret stmts 3
            in result `shouldBe` expected


        {-
        x = input;
        r = 0;
        i = 0;
        while i < x {
            f = 0;
            s = 1;
            j = 0;
            t = 0;
            while j < i {
                t = f;
                f = s;
                s = t + f;
                j = j + 1;
            }
            r = r + s;
            i = i + 1;
        }
        return r;
        -} 

        it "PAInt: sum((0 to 10).map(fib(_)))" $ 
            let input   = Var "input"
                x       = Var "x"
                r       = Var "r"
                f       = Var "f"
                s       = Var "s"
                i       = Var "i"
                j       = Var "j"
                t       = Var "t"
                b       = Var "b"
                stmts   = [
                    Assign x (VarExp input),
                    Assign r (ConstExp (IntConst 0)),
                    Assign i (ConstExp (IntConst 0)),
                    While (LThan (VarExp i)  (VarExp x)) [
                        Assign f (ConstExp (IntConst 0)),
                        Assign s (ConstExp (IntConst 1)),
                        Assign j (ConstExp (IntConst 0)),
                        Assign t (ConstExp (IntConst 0)),
                        While (LThan (VarExp j) (VarExp i)) [
                            Assign t (VarExp f),
                            Assign f (VarExp s),
                            Assign s (Plus (VarExp t) (VarExp f)),
                            Assign j (Plus (VarExp j) (ConstExp (IntConst 1)))
                        ],
                        Assign r (Plus (VarExp r) (VarExp s)),
                        Assign i (Plus (VarExp i) (ConstExp (IntConst 1)))],
                    Ret r]
                expected = Right (IntConst 7)
                result = interpret stmts 4
            in result `shouldBe` expected