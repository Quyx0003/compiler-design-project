module Lang.Simp.Semantics.TypeInfSpec where 

import Test.Hspec
import qualified Data.Set as DS
import qualified Data.Map as DM 
import Data.Either
import Lang.Simp.Syntax.AST
import Lang.Simp.Semantics.TypeInf

spec :: Spec
spec = do
    describe "TypeInf" $ do
            
        it "substitution [int/a]a" $ 
            let psi         = RevComp ("a", MonoType IntTy) Empty 
                expected    = MonoType IntTy
            in applySubst psi (TypeVar "a") `shouldBe` expected

        it "substitution [int/a]b" $ 
            let psi         = RevComp ("a", MonoType IntTy) Empty 
                expected    = TypeVar "b"
            in applySubst psi (TypeVar "b") `shouldBe` expected

        it "substitution [int/a]bool" $ 
            let psi         = RevComp ("a", MonoType IntTy) Empty 
                expected    = MonoType BoolTy
            in applySubst psi (MonoType BoolTy) `shouldBe` expected

        it "substitution ([int/a]o[a/b])b" $ 
            let psi         = RevComp ("b",TypeVar "a") (RevComp ("a",MonoType IntTy) Empty)
                expected    = MonoType IntTy 
            in applySubst psi (TypeVar "b") `shouldBe` expected  

        it "unification (Int,Bool) should fail." $ 
            let tyconstrs   = DS.singleton (MonoType IntTy, MonoType BoolTy)
            in mgu tyconstrs `shouldSatisfy` isLeft

        it "unification (Int,a) should ground a." $ 
            let tyconstrs   = DS.singleton (MonoType IntTy, TypeVar "a")
            in mgu tyconstrs `shouldSatisfy` (\r -> case r of
                Left _        -> False 
                Right tySubst -> applySubst tySubst (TypeVar "a") == MonoType IntTy
            )  

        it "unification {(Int,a), (a, b)} should ground b." $ 
            let tyconstrs   = DS.fromList [(MonoType IntTy, TypeVar "a"), (TypeVar "a" , (TypeVar "b"))]
            in mgu tyconstrs `shouldSatisfy` (\r -> case r of
                Left _        -> False 
                Right tySubst -> applySubst tySubst (TypeVar "b") == MonoType IntTy
            )  

        it "unification {(a, b), (Int, Int) (Int,a)} should ground b." $ 
            let tyconstrs   = DS.fromList [(TypeVar "a", TypeVar "b"), (MonoType IntTy, MonoType IntTy), (MonoType IntTy, TypeVar "a")]
            in mgu tyconstrs `shouldSatisfy` (\r -> case r of
                Left _        -> False 
                Right tySubst -> applySubst tySubst (TypeVar "b") == MonoType IntTy
            )  

        it "typeinf \ 
            \x = input;\n\
            \s = 0;\n\
            \c = 0;\n\
            \while c < x {\n\
            \  s = c + s;\n\
            \  c = c + 1;\n\
            \  }\n\
            \return s;" $
            let stmts = [Assign (Var "x") (VarExp (Var "input")), 
                        Assign (Var "s") (ConstExp (IntConst 0)), 
                        Assign (Var "c") (ConstExp (IntConst 0)), 
                        While (LThan (VarExp (Var "c")) (VarExp (Var "x"))) [
                            Assign (Var "s") (Plus (VarExp (Var "c")) (VarExp (Var "s"))), 
                            Assign (Var "c") (Plus (VarExp (Var "c")) (ConstExp (IntConst 1)))
                        ],
                        Ret (Var "s")]
                expected = Right $ DM.fromList [(Var "c", IntTy), (Var "x", IntTy), (Var "input", IntTy), (Var "s", IntTy)]
            in typeInf stmts `shouldBe` expected  



        it "typeinf fib" $
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
                expected = Right $ DM.fromList [ (Var "f", IntTy), (Var "input", IntTy), (Var "x", IntTy), (Var "s", IntTy), (Var "c", IntTy), (Var "t", IntTy)]
            in typeInf stmts `shouldBe` expected  

        {-
            x = input;          // (α_x, α_input)      
            y = 0;              // (α_y, int)
            while (y < 3) {     // (α_y, int)
                y = y + 1;      // (α_y, int)
            }
            return y; 
        -}
        it "typeinf should fail in grounding input and x's types" $ 
            let input   = Var "input"
                x       = Var "x"
                y       = Var "y"
                p       = [ Assign x (VarExp input),
                            Assign y (ConstExp (IntConst 0)),
                            While (LThan (VarExp y) (ConstExp (IntConst 3))) [
                                Assign y (Plus (VarExp y) (ConstExp (IntConst 1)))
                            ],
                            Ret y]
            in typeInf p `shouldSatisfy` (\r -> case r of
                Left err -> True
                Right _  -> False 
                )


        {-
            x = input;          // (α_x, α_input)      
            y = 0;              // (α_y, int)
            while (y - x) {     // (α_y, int), (α_x,int)
                y = y + 1;      // (α_y, int)
            }
            return y;
        -}
        it "typeinf should fail in unifying int with bool" $ 
            let input   = Var "input"
                x       = Var "x"
                y       = Var "y"
                p       = [ Assign x (VarExp input),
                            Assign y (ConstExp (IntConst 0)),
                            While (Minus (VarExp y) (VarExp x)) [
                                Assign y (Plus (VarExp y) (ConstExp (IntConst 1)))
                            ],
                            Ret y]
            in typeInf p `shouldSatisfy` (\r -> case r of
                Left err -> True
                Right _  -> False 
                )

