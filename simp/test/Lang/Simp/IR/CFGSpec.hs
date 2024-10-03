module Lang.Simp.IR.CFGSpec where 

import Test.Hspec
import Control.Monad.State
import Lang.Simp.IR.PseudoAssembly
import Lang.Simp.IR.CFG
import Lang.Simp.IR.Util
import qualified Data.Map as DM

spec :: Spec
spec = do
    describe "cfg" $ do

        {- 1: x <- 0
           2: y <- 10
           3: i <- 0
           4: var_1 <- i < y
           5: ifn var_1 goto 11
           6: var_2 <- x + i
           7: x <- var_2
           8: var_3 <- i + 1
           9: i <- var_3
           10: goto 4
           11: r_ret <- x
           12: ret
        -}
        it "test buildCFG: 1" $ 
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
                expected = DM.fromList [(1 , [2]), (2 , [3]), (3 , [4]), (4 , [5]), (5 , [6, 11]), (6 , [7]), (7 , [8]),
                            (8 , [9]), (9 , [10]), (10 , [4]), (11 , [12])]
                result = buildCFG pa
            in result `shouldBe` expected 

        {-
        1: x <- input
        2: f <- 0
        3: s <- 1
        4: c <- 0
        5: t <- 0
        6: b <- c < x
        7: ifn b goto 13
        8: t <- f
        9: f <- s
        10: s <- t + f
        11: c <- c + 1
        12: goto 6
        13: r_ret <- s
        14: ret
        -}
        it "test buildCFG: 2" $ 
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
                expected = DM.fromList [(1 , [2]), (2 , [3]), (3 , [4]), (4 , [5]), 
                            (5 , [6]), (6 , [7]), (7 , [8,13]), (8 , [9]), (9 , [10]), 
                            (10 , [11]), (11 , [12]), (12, [6]), (13, [14])]
                result = buildCFG pa
            in result `shouldBe` expected 

        {-
        1: x <- input
        2: r <- 0
        3: i <- 0
        4: b <- i < x
        5: ifn b goto 20
        6: f <- 0
        7: s <- 1
        8: j <- 0
        9: t <- 0
        10: b <- j < i
        11: ifn b goto 17
        12: t <- f
        13: f <- s
        14: s <- t + f
        15: j <- j + 1
        16: goto 10
        17: r <- r + s
        18: i <- i + 1
        19: goto 4
        20: _ret_r <- r
        21: ret
        -}
        it "test buildCFG: 3" $ 
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
                expected = DM.fromList [(1 , [2]), (2 , [3]), (3 , [4]), (4 , [5]), 
                            (5 , [6, 20]), (6 , [7]), (7 , [8]), (8 , [9]), 
                            (9 , [10]), (10 , [11]), (11 , [12, 17]), (12, [13]), 
                            (13, [14]), (14, [15]), (15, [16]), (16, [10]),
                            (17, [18]), (18, [19]), (19, [4]), (20, [21])]
                result = buildCFG pa
            in result `shouldBe` expected 