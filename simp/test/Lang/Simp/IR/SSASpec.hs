module Lang.Simp.IR.SSASpec where 

import Test.Hspec
import Control.Monad.State
import Lang.Simp.IR.PseudoAssembly
import Lang.Simp.IR.CFG
import Lang.Simp.IR.Util
import Lang.Simp.IR.SSA
import qualified Data.Map as DM

spec :: Spec
spec = do
    describe "ssa" $ do


        it "test insertPhis " $ 
            let input = Temp (AVar "input")
                x     = Temp (AVar "x") 
                f     = Temp (AVar "f")
                s     = Temp (AVar "s")
                c     = Temp (AVar "c")
                b     = Temp (AVar "b")
                i     = Temp (AVar "i")
                j     = Temp (AVar "j")
                t     = Temp (AVar "t")
                r     = Temp (AVar "r")
                r_ret = Regstr "_r_ret"
                pa = [
                        (1, IMove x input),
                        (2, IMove r (IntLit 0)),
                        (3, IMove i (IntLit 0)),
                        (4, ILThan b i x),
                        (5, IIfNot b 20),
                        (6, IMove f (IntLit 0)),
                        (7, IMove s (IntLit 1)),
                        (8, IMove j (IntLit 0)),
                        (9, IMove t (IntLit 0)),
                        (10, ILThan b j i),
                        (11, IIfNot b 17),
                        (12, IMove t f),
                        (13, IMove f s),
                        (14, IPlus s t f),
                        (15, IPlus j j (IntLit 1)),
                        (16, IGoto 10),
                        (17, IPlus r r s),
                        (18, IPlus i i (IntLit 1)),
                        (19, IGoto 4),
                        (20, IMove r_ret r),
                        (21, IRet) ]
                cfg = DM.fromList [(1 , [2]), (2 , [3]), (3 , [4]), (4 , [5]), 
                            (5 , [6, 20]), (6 , [7]), (7 , [8]), (8 , [9]), 
                            (9 , [10]), (10 , [11]), (11 , [12, 17]), (12, [13]), 
                            (13, [14]), (14, [15]), (15, [16]), (16, [10]),
                            (17, [18]), (18, [19]), (19, [4]), (20, [21])]
                dft = DM.fromList [(1,[]),(2,[]),(3,[]),(4,[4]),(5,[4]),(6,[4]),(7,[4]),(8,[4]),(9,[4]),(10,[4,10]),(11,[4,10]),(12,[10]),(13,[10]),(14,[10]),(15,[10]),(16,[10]),(17,[4]),(18,[4]),(19,[4]),(20,[]),(21,[])]
                expected = DM.fromList [(1,(1,[],IMove (Temp (AVar {name = "x"})) (Temp (AVar {name = "input"})))),(2,(2,[],IMove (Temp (AVar {name = "r"})) (IntLit 0))),(3,(3,[],IMove (Temp (AVar {name = "i"})) (IntLit 0))),(4,(4,[PhiAssignment (Temp (AVar {name = "b"})) [(3,AVar {name = "b"}),(19,AVar {name = "b"})] (Temp (AVar {name = "b"})),PhiAssignment (Temp (AVar {name = "f"})) [(3,AVar {name = "f"}),(19,AVar {name = "f"})] (Temp (AVar {name = "f"})),PhiAssignment (Temp (AVar {name = "i"})) [(3,AVar {name = "i"}),(19,AVar {name = "i"})] (Temp (AVar {name = "i"})),PhiAssignment (Temp (AVar {name = "j"})) [(3,AVar {name = "j"}),(19,AVar {name = "j"})] (Temp (AVar {name = "j"})),PhiAssignment (Temp (AVar {name = "r"})) [(3,AVar {name = "r"}),(19,AVar {name = "r"})] (Temp (AVar {name = "r"})),PhiAssignment (Temp (AVar {name = "s"})) [(3,AVar {name = "s"}),(19,AVar {name = "s"})] (Temp (AVar {name = "s"})),PhiAssignment (Temp (AVar {name = "t"})) [(3,AVar {name = "t"}),(19,AVar {name = "t"})] (Temp (AVar {name = "t"}))],ILThan (Temp (AVar {name = "b"})) (Temp (AVar {name = "i"})) (Temp (AVar {name = "x"})))),(5,(5,[],IIfNot (Temp (AVar {name = "b"})) 20)),(6,(6,[],IMove (Temp (AVar {name = "f"})) (IntLit 0))),(7,(7,[],IMove (Temp (AVar {name = "s"})) (IntLit 1))),(8,(8,[],IMove (Temp (AVar {name = "j"})) (IntLit 0))),(9,(9,[],IMove (Temp (AVar {name = "t"})) (IntLit 0))),(10,(10,[PhiAssignment (Temp (AVar {name = "b"})) [(9,AVar {name = "b"}),(16,AVar {name = "b"})] (Temp (AVar {name = "b"})),PhiAssignment (Temp (AVar {name = "f"})) [(9,AVar {name = "f"}),(16,AVar {name = "f"})] (Temp (AVar {name = "f"})),PhiAssignment (Temp (AVar {name = "j"})) [(9,AVar {name = "j"}),(16,AVar {name = "j"})] (Temp (AVar {name = "j"})),PhiAssignment (Temp (AVar {name = "s"})) [(9,AVar {name = "s"}),(16,AVar {name = "s"})] (Temp (AVar {name = "s"})),PhiAssignment (Temp (AVar {name = "t"})) [(9,AVar {name = "t"}),(16,AVar {name = "t"})] (Temp (AVar {name = "t"}))],ILThan (Temp (AVar {name = "b"})) (Temp (AVar {name = "j"})) (Temp (AVar {name = "i"})))),(11,(11,[],IIfNot (Temp (AVar {name = "b"})) 17)),(12,(12,[],IMove (Temp (AVar {name = "t"})) (Temp (AVar {name = "f"})))),(13,(13,[],IMove (Temp (AVar {name = "f"})) (Temp (AVar {name = "s"})))),(14,(14,[],IPlus (Temp (AVar {name = "s"})) (Temp (AVar {name = "t"})) (Temp (AVar {name = "f"})))),(15,(15,[],IPlus (Temp (AVar {name = "j"})) (Temp (AVar {name = "j"})) (IntLit 1))),(16,(16,[],IGoto 10)),(17,(17,[],IPlus (Temp (AVar {name = "r"})) (Temp (AVar {name = "r"})) (Temp (AVar {name = "s"})))),(18,(18,[],IPlus (Temp (AVar {name = "i"})) (Temp (AVar {name = "i"})) (IntLit 1))),(19,(19,[],IGoto 4)),(20,(20,[],IMove (Regstr "_r_ret") (Temp (AVar {name = "r"})))),(21,(21,[],IRet))]
                result = insertPhis pa dft cfg 
            in result `shouldBe` expected
        it "test buildSSA: 1" $ 
            let pa = [
                        (1, IMove (Temp (AVar "x")) (IntLit 0)), 
                        (2, IMove (Temp (AVar "y")) (IntLit 10)), 
                        (3, IMove (Temp (AVar "i")) (IntLit 0)), 
                        (4, ILThan (Temp (AVar "var_1")) (Temp (AVar "i")) (Temp (AVar "y"))), 
                        (5, IIfNot (Temp (AVar "var_1")) 11), 
                        (6, IPlus (Temp (AVar "var_2")) (Temp (AVar "x")) (Temp (AVar "i"))), 
                        (7, IMove (Temp (AVar "x")) (Temp (AVar "var_2"))), 
                        (8, IPlus (Temp (AVar "var_3")) (Temp (AVar "i")) (IntLit 1)), 
                        (9, IMove (Temp (AVar "i")) (Temp (AVar "var_3"))), 
                        (10,IGoto 4), 
                        (11,IMove (Regstr "_r_ret") (Temp (AVar "x"))), 
                        (12,IRet) ]
                expected = Right [
                        (1,[],IMove (Temp (AVar "x_1")) (IntLit 0)), 
                        (2,[],IMove (Temp (AVar "y_1")) (IntLit 10)), 
                        (3,[],IMove (Temp (AVar "i_1")) (IntLit 0)), 
                        (4,[
                            PhiAssignment (Temp (AVar "i_2")) [(3, AVar "i_1"), (10, AVar "i_3")] (Temp (AVar "i")), 
                            PhiAssignment (Temp (AVar "x_2")) [(3, AVar "x_1"), (10, AVar "x_3")] (Temp (AVar "x"))
                            ],ILThan (Temp (AVar "var_1_1")) (Temp (AVar "i_2")) (Temp (AVar "y_1"))), 
                        (5,[],IIfNot (Temp (AVar "var_1_1")) 11), 
                        (6,[],IPlus (Temp (AVar "var_2_1")) (Temp (AVar "x_2")) (Temp (AVar "i_2"))), 
                        (7,[],IMove (Temp (AVar "x_3")) (Temp (AVar "var_2_1"))), 
                        (8,[],IPlus (Temp (AVar "var_3_1")) (Temp (AVar "i_2")) (IntLit 1)), 
                        (9,[],IMove (Temp (AVar "i_3")) (Temp (AVar "var_3_1"))), 
                        (10,[],IGoto 4), 
                        (11,[],IMove (Regstr "_r_ret") (Temp (AVar "x_2"))), 
                        (12,[],IRet) ]
                result = buildSSA pa
            in result `shouldBe` expected 

        it "test buildSSA: 2" $ 
            let input = Temp (AVar "input")
                x     = Temp (AVar "x") 
                f     = Temp (AVar "f")
                s     = Temp (AVar "s")
                c     = Temp (AVar "c")
                b     = Temp (AVar "b")
                t     = Temp (AVar "t")
                r_ret = Regstr "_r_ret"
                pa = [
                        (1, IMove x input), 
                        (2, IMove f (IntLit 0)), 
                        (3, IMove s (IntLit 1)),
                        (4, IMove c (IntLit 0)),
                        (5, IMove t (IntLit 0)),
                        (6, ILThan b c x),
                        (7, IIfNot b 13),
                        (8, IMove t f),
                        (9, IMove f s),
                        (10, IPlus s t f),
                        (11, IPlus c c (IntLit 1)),
                        (12, IGoto 6),
                        (13, IMove r_ret s),
                        (14, IRet)]
                expected = Right [ 
                            (1,[],IMove (Temp (AVar "x_1")) (Temp(AVar "input"))), 
                            (2,[],IMove (Temp (AVar "f_1")) (IntLit 0)), 
                            (3,[],IMove (Temp (AVar "s_1")) (IntLit 1)), 
                            (4,[],IMove (Temp (AVar "c_1")) (IntLit 0)), 
                            (5,[],IMove (Temp (AVar "t_1")) (IntLit 0)), 
                            (6,[
                                PhiAssignment (Temp (AVar "c_2")) [(5,AVar "c_1"), (12,AVar "c_3")] (Temp (AVar "c")), 
                                PhiAssignment (Temp (AVar "f_2")) [(5,AVar "f_1"), (12,AVar "f_3")] (Temp (AVar "f")), 
                                PhiAssignment (Temp (AVar "s_2")) [(5,AVar "s_1"), (12,AVar "s_3")] (Temp (AVar "s")),
                                PhiAssignment (Temp (AVar "t_2")) [(5,AVar "t_1"), (12,AVar "t_3")] (Temp (AVar "t")) 
                                ], ILThan (Temp (AVar "b_1")) (Temp (AVar "c_2")) (Temp (AVar "x_1"))), 
                            (7,[],IIfNot (Temp (AVar "b_1")) 13), 
                            (8,[],IMove (Temp (AVar "t_3")) (Temp (AVar "f_2"))), 
                            (9,[],IMove (Temp (AVar "f_3")) (Temp (AVar "s_2"))), 
                            (10,[],IPlus (Temp (AVar "s_3")) (Temp (AVar "t_3")) (Temp (AVar "f_3"))), 
                            (11,[],IPlus (Temp (AVar "c_3")) (Temp (AVar "c_2")) (IntLit 1)), 
                            (12,[],IGoto 6), 
                            (13,[],IMove (Regstr "_r_ret") (Temp (AVar "s_2"))), 
                            (14,[],IRet) ]
                result = buildSSA pa
            in result `shouldBe` expected 

        it "test buildSSA: 3" $ 
            let input = Temp (AVar "input")
                x     = Temp (AVar "x") 
                f     = Temp (AVar "f")
                s     = Temp (AVar "s")
                c     = Temp (AVar "c")
                b     = Temp (AVar "b")
                i     = Temp (AVar "i")
                j     = Temp (AVar "j")
                t     = Temp (AVar "t")
                r     = Temp (AVar "r")
                r_ret = Regstr "_r_ret"
                pa = [
                        (1, IMove x input),
                        (2, IMove r (IntLit 0)),
                        (3, IMove i (IntLit 0)),
                        (4, ILThan b i x),
                        (5, IIfNot b 20),
                        (6, IMove f (IntLit 0)),
                        (7, IMove s (IntLit 1)),
                        (8, IMove j (IntLit 0)),
                        (9, IMove t (IntLit 0)),
                        (10, ILThan b j i),
                        (11, IIfNot b 17),
                        (12, IMove t f),
                        (13, IMove f s),
                        (14, IPlus s t f),
                        (15, IPlus j j (IntLit 1)),
                        (16, IGoto 10),
                        (17, IPlus r r s),
                        (18, IPlus i i (IntLit 1)),
                        (19, IGoto 4),
                        (20, IMove r_ret r),
                        (21, IRet) ]
                expected = Right [ 
                                (1,[],IMove (Temp (AVar "x_1")) (Temp (AVar "input"))), 
                                (2,[],IMove (Temp (AVar "r_1")) (IntLit 0)), 
                                (3,[],IMove (Temp (AVar "i_1")) (IntLit 0)), 
                                (4,[
                                    PhiAssignment (Temp(AVar "i_2")) [(3,AVar "i_1"), (19, AVar "i_3")] (Temp (AVar "i")),
                                    PhiAssignment (Temp(AVar "r_2")) [(3,AVar "r_1"), (19, AVar "r_3")] (Temp (AVar "r")) 
                                    ], ILThan (Temp(AVar "b_1")) (Temp (AVar "i_2")) (Temp (AVar "x_1"))), 
                                (5,[],IIfNot (Temp (AVar "b_1")) 20), 
                                (6,[],IMove  (Temp (AVar "f_1")) (IntLit 0)), 
                                (7,[],IMove  (Temp (AVar "s_1")) (IntLit 1)), 
                                (8,[],IMove  (Temp (AVar "j_1")) (IntLit 0)), 
                                (9,[],IMove (Temp (AVar "t_1")) (IntLit 0)), 
                                (10,[
                                    PhiAssignment (Temp (AVar "b_2")) [(9,AVar "b_1"), (16,AVar "b_3")] (Temp (AVar "b")), 
                                    PhiAssignment (Temp (AVar "f_2")) [(9,AVar "f_1"), (16,AVar "f_3")] (Temp (AVar "f")), 
                                    PhiAssignment (Temp (AVar "j_2")) [(9,AVar "j_1"), (16,AVar "j_3")] (Temp (AVar "j")), 
                                    PhiAssignment (Temp (AVar "s_2")) [(9,AVar "s_1"), (16,AVar "s_3")] (Temp (AVar "s")),
                                    PhiAssignment (Temp (AVar "t_2")) [(9,AVar "t_1"), (16,AVar "t_3")] (Temp (AVar "t")) 
                                    ],ILThan (Temp (AVar "b_3")) (Temp (AVar "j_2")) (Temp (AVar "i_2"))), 
                                (11,[],IIfNot (Temp (AVar "b_3")) 17), 
                                (12,[],IMove (Temp (AVar "t_3")) (Temp (AVar "f_2"))), 
                                (13,[],IMove (Temp (AVar "f_3")) (Temp (AVar "s_2"))), 
                                (14,[],IPlus (Temp (AVar "s_3")) (Temp (AVar "t_3")) (Temp (AVar "f_3"))), 
                                (15,[],IPlus (Temp (AVar "j_3")) (Temp (AVar "j_2")) (IntLit 1)), 
                                (16,[],IGoto 10), 
                                (17,[],IPlus (Temp (AVar "r_3")) (Temp (AVar "r_2")) (Temp (AVar "s_2"))), 
                                (18,[],IPlus (Temp (AVar "i_3")) (Temp (AVar "i_2")) (IntLit 1)), 
                                (19,[],IGoto 4), 
                                (20,[],IMove (Regstr "_r_ret") (Temp (AVar "r_2"))), 
                                (21,[],IRet)]
                result = buildSSA pa
            in result `shouldBe` expected 