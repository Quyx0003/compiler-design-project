module Lang.Simp.Interpreter.PAIntSpec where 

import Test.Hspec
import qualified Data.Map as DM
import Lang.Simp.IR.PseudoAssembly
import Lang.Simp.Interpreter.PAInt 

spec :: Spec
spec = do
    describe "PAInt" $ do

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

        it "PAInt: fib(4)" $ 
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
                expected = Right 5
                result = interpret pa 4
            in result `shouldBe` expected

        {-
        1: x <- input
        2: s <- 0
        3: c <- 0
        4: b <- c < x
        5: ifn b goto 9
        6: s <- c + s
        7: c <- c + 1
        8: goto 4
        9: _ret_r <- s
        10: ret
        -} 
        it "PAInt: sum(3)" $ 
            let input   = Temp (AVar "input")
                x       = Temp (AVar "x")
                s       = Temp (AVar "s")
                c       = Temp (AVar "c")
                b       = Temp (AVar "b")
                r_ret   = Regstr "_r_ret"
                pa      = [
                            (1, IMove x input),
                            (2, IMove s (IntLit 0)),
                            (3, IMove c (IntLit 0)),
                            (4, ILThan b c x),
                            (5, IIfNot b 9),
                            (6, IPlus s c s),
                            (7, IPlus c c (IntLit 1)),
                            (8, IGoto 4),
                            (9, IMove r_ret s),
                            (10, IRet) ]
                expected = Right 3
                result = interpret pa 3
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

        it "PAInt: sum((0 to 10).map(fib(_)))" $ 
            let input   = Temp (AVar "input")
                x       = Temp (AVar "x")
                r       = Temp (AVar "r")
                f       = Temp (AVar "f")
                s       = Temp (AVar "s")
                i       = Temp (AVar "i")
                j       = Temp (AVar "j")
                t       = Temp (AVar "t")
                b       = Temp (AVar "b")
                r_ret   = Regstr "_r_ret"
                pa      = [
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
                            (21, IRet)]
                expected = Right 7
                result = interpret pa 4
            in result `shouldBe` expected