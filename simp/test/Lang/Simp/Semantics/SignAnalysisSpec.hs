module Lang.Simp.Semantics.SignAnalysisSpec where 

import Test.Hspec
import qualified Data.Set as DS
import qualified Data.Map as DM 
import Data.Either
import Lang.Simp.IR.PseudoAssembly
import Lang.Simp.Lattice.SignLattice
import Lang.Simp.Semantics.SignAnalysis

spec :: Spec
spec = do
    describe "sign analysis" $ do
            
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
        it "test sign analysis with sum()" $ 
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
                    (1 , DM.fromList [("b", Top), ("c", Top), ("input", Top), ("s", Top), ("x", Top)]), 
                    (2 , DM.fromList [("b", Top), ("c", Top), ("input", Top), ("s", Zero), (("x", Top))]), 
                    (3 , DM.fromList [("b", Top), ("c", Zero),("input", Top),("s", Zero), ("x", Top)]), 
                    (4 , DM.fromList [("b", Top), ("c", Top), ("input", Top), ("s", Top), ("x", Top)]),
                    (5 , DM.fromList [("b", Top), ("c", Top), ("input", Top), ("s", Top), ("x", Top)]), 
                    (6 , DM.fromList [("b", Top), ("c", Top), ("input", Top), ("s", Top), ("x", Top)]), 
                    (7 , DM.fromList [("b", Top), ("c", Top), ("input", Top), ("s", Top), ("x", Top)]), 
                    (8 , DM.fromList [("b", Top), ("c", Top), ("input", Top), ("s", Top), ("x", Top)]), 
                    (9 , DM.fromList [("b", Top), ("c", Top), ("input", Top), ("s", Top), ("x", Top)]), 
                    (10 , DM.fromList [("b", Top), ("c", Top), ("input", Top), ("s", Top), ("x", Top)])]
            
            in analyze pa  `shouldBe` expected

        -- This test case shows that this sign analysis is not accurate enough to detect the negativity of x of return statement
        {- 
        // SIMP1
        x = input;
        while (x >= 0) {
            x = x - 1;
        }
        return x; // x must be -

        1: x <- input
        2: a <- 0 == x
        3: b <- 0 < x
        4: c <- a + b   // b || c
        5: ifn c goto 8
        6: x <- x - 1
        7: goto 2
        8: _ret_r <- x
        9: ret        
        -}

        it "test sign analysis with with descending subtraction" $ 
            let input   = Temp (AVar "input")
                x       = Temp (AVar "x")
                a       = Temp (AVar "a")
                c       = Temp (AVar "c")
                b       = Temp (AVar "b")
                r_ret   = Regstr "_r_ret"
                pa      = [ (1, IMove x input),
                            (2, IDEqual a (IntLit 0) x),
                            (3, ILThan b (IntLit 0) x),
                            (4, ILThan c a b),
                            (5, IIfNot c 8),
                            (6, IMinus x x (IntLit 1)),
                            (7, IGoto 2),
                            (8, IMove r_ret x),
                            (9, IRet)]
                expected = Right $ DM.fromList [
                    (1 , DM.fromList [("x", Top), ("a", Top), ("b", Top), ("c", Top), ("input", Top)]), 
                    (2 , DM.fromList [("x", Top), ("a", Top), ("b", Top), ("c", Top), ("input", Top)]), 
                    (3 , DM.fromList [("x", Top), ("a", Top), ("b", Top), ("c", Top), ("input", Top)]), 
                    (4 , DM.fromList [("x", Top), ("a", Top), ("b", Top), ("c", Top), ("input", Top)]),
                    (5 , DM.fromList [("x", Top), ("a", Top), ("b", Top), ("c", Top), ("input", Top)]), 
                    (6 , DM.fromList [("x", Top), ("a", Top), ("b", Top), ("c", Top), ("input", Top)]), 
                    (7 , DM.fromList [("x", Top), ("a", Top), ("b", Top), ("c", Top), ("input", Top)]), 
                    (8 , DM.fromList [("x", Top), ("a", Top), ("b", Top), ("c", Top), ("input", Top)]), 
                    (9 , DM.fromList [("x", Top), ("a", Top), ("b", Top), ("c", Top), ("input", Top)])]
            
            in analyze pa  `shouldBe` expected
