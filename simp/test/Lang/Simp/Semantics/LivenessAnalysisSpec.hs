module Lang.Simp.Semantics.LivenessAnalysisSpec where 

import Test.Hspec
import qualified Data.Set as DS
import qualified Data.Map as DM 
import Data.Either
import Lang.Simp.IR.PseudoAssembly
import Lang.Simp.Semantics.LivenessAnalysis

spec :: Spec
spec = do
    describe "liveness analysis" $ do
            
        {- 
        1: x <- inpput
        2: y <- x + 1
        3: z <- y + 1
        4: w <- y * z
        5: _r_ret <- w
        6: ret
        -}

        it "test liveness analysis with a simple example" $ 
            let input   = Temp (AVar "input")
                x       = Temp (AVar "x")
                y       = Temp (AVar "y")
                z       = Temp (AVar "z")
                w       = Temp (AVar "w")
                r_ret   = Regstr "_r_ret"
                pa      = [
                            (1, IMove x input),
                            (2, IPlus y x (IntLit 1)),
                            (3, IPlus z y (IntLit 1)),
                            (4, IMult w y z),
                            (5, IMove r_ret w),
                            (6, IRet)]
                expected = Right $ DM.fromList [
                            (1 , DS.fromList ["input"]), 
                            (2 , DS.fromList ["x"]), 
                            (3 , DS.fromList ["y"]), 
                            (4 , DS.fromList ["y", "z"]),
                            (5 , DS.fromList ["w"]),
                            (6 , DS.empty) ]
            in analyze pa  `shouldBe` expected


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
        it "test liveness analysis with sum()" $ 
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
                expected = Right $ DM.fromList [
                            (1 , DS.fromList ["input"]), 
                            (2 , DS.fromList ["x"]), 
                            (3 , DS.fromList ["s", "x"]), 
                            (4 , DS.fromList ["c", "s", "x"]),
                            (5 , DS.fromList ["x", "c", "s", "b"]), 
                            (6 , DS.fromList ["x", "c", "s"]), 
                            (7 , DS.fromList ["s", "x", "c"]), 
                            (8 , DS.fromList ["c", "s", "x"]), 
                            (9 , DS.fromList ["s"]), 
                            (10 ,DS.empty) ]
            in analyze pa  `shouldBe` expected

        {-
        1: x <- input
        2: y <- 0
        3: s <- 0
        4: b <- y < x
        5: ifn b goto 10
        6: y <- y + 1
        7: t <- s
        8: s <- s + y
        9: goto 4
        10: rret <- s
        11: ret            
        -}
        it "test liveness analysis with notes example" $ 
            let input   = Temp (AVar "input")
                x       = Temp (AVar "x")
                y       = Temp (AVar "y")
                s       = Temp (AVar "s")
                t       = Temp (AVar "t")
                b       = Temp (AVar "b")
                r_ret   = Regstr "_r_ret"
                pa      = [
                            (1, IMove x input),
                            (2, IMove y (IntLit 0)),
                            (3, IMove s (IntLit 0)),
                            (4, ILThan b y x),
                            (5, IIfNot b 10),
                            (6, IPlus y y (IntLit 1)),
                            (7, IMove t s),
                            (8, IPlus s s y),
                            (9, IGoto 4),
                            (10, IMove r_ret s),
                            (11, IRet) ]
                expected = Right $ DM.fromList [
                            (1 , DS.fromList ["input"]), 
                            (2 , DS.fromList ["x"]), 
                            (3 , DS.fromList ["y", "x"]), 
                            (4 , DS.fromList ["s", "y", "x"]),
                            (5 , DS.fromList ["x", "s", "y", "b"]), 
                            (6 , DS.fromList ["y", "x", "s"]), 
                            (7 , DS.fromList ["y", "x", "s"]), 
                            (8 , DS.fromList ["y", "x", "s"]), 
                            (9 , DS.fromList ["y", "x", "s"]), 
                            (10 , DS.fromList ["s"]), 
                            (11 ,DS.empty) ]
            in analyze pa  `shouldBe` expected


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
        it "test liveness analysis with fib()" $ 
            let input   = Temp (AVar "input")
                x       = Temp (AVar "x")
                f       = Temp (AVar "f")
                s       = Temp (AVar "s")
                c       = Temp (AVar "c")
                b       = Temp (AVar "b")
                t       = Temp (AVar "t")
                r_ret   = Regstr "_r_ret"
                pa      = [
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
                expected = Right $ DM.fromList [
                    (1 , DS.fromList ["input"]), 
                    (2 , DS.fromList ["x"]), 
                    (3 , DS.fromList ["f", "x"]), 
                    (4 , DS.fromList ["s", "f", "x"]),
                    (5 , DS.fromList ["s", "f", "c", "x"]), 
                    (6 , DS.fromList ["c", "s", "f", "x"]), 
                    (7 , DS.fromList ["f", "b", "c", "s", "x"]), 
                    (8 , DS.fromList ["x", "c", "s", "f"]), 
                    (9 , DS.fromList ["x", "c", "t", "s"]), 
                    (10 , DS.fromList ["f", "x", "c", "t"]), 
                    (11 , DS.fromList ["s", "f", "x", "c"]), 
                    (12 , DS.fromList ["c", "s", "f", "x"]), 
                    (13 , DS.fromList ["s"]), 
                    (14 , DS.fromList [])]
            in analyze pa  `shouldBe` expected            