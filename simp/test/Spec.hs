-- {-# OPTIONS_GHC -F -pgmF hspec-discover #-}
-- the above does not work with haskell language server unless hspec-discover is in the path
-- doing it manually 

import Test.Hspec
import qualified Lang.Simp.Syntax.LexerSpec as LexerSpec
import qualified Lang.Simp.Syntax.ParserSpec as ParserSpec
import qualified Lang.Simp.IR.MaximalMunchSpec as MaximalMunchSpec
import qualified Lang.Simp.IR.MMUpDownSpec as MMUpDownSpec
import qualified Lang.Simp.IR.CFGSpec as CFGSpec
import qualified Lang.Simp.IR.DFSpec as DFSpec
import qualified Lang.Simp.IR.SSASpec as SSASpec
import qualified Lang.Simp.Semantics.TypeInfSpec as TypeInfSpec 
import qualified Lang.Simp.Semantics.SignAnalysisSpec as SignAnalysisSpec 
import qualified Lang.Simp.Semantics.LivenessAnalysisSpec as LivenessAnalysisSpec 
import qualified Lang.Simp.Interpreter.PAIntSpec as PAIntSpec
import qualified Lang.Simp.Interpreter.SimpIntSpec as SimpIntSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "Lang.Simp.Syntax.LexerSpec" LexerSpec.spec
    describe "Lang.Simp.Syntax.ParserSpec" ParserSpec.spec
    describe "Lang.Simp.IR.MaximalMunchSpec" MaximalMunchSpec.spec
    describe "Lang.Simp.IR.MMUpDownSpec" MMUpDownSpec.spec
    describe "Lang.Simp.IR.CFGSpec" CFGSpec.spec
    describe "Lang.Simp.IR.DFSpec" DFSpec.spec
    describe "Lang.Simp.IR.SSASpec" SSASpec.spec
    describe "Lang.Simp.Semantics.TypeInfSpec" TypeInfSpec.spec 
    describe "Lang.Simp.Semantics.SignAnalysisSpec" SignAnalysisSpec.spec 
    describe "Lang.Simp.Semantics.LivenessAnalysisSpec" LivenessAnalysisSpec.spec 
    describe "Lang.Simp.Interpreter.PAIntSpec" PAIntSpec.spec 
    describe "Lang.Simp.Interpreter.SimpIntSpec" SimpIntSpec.spec 

-- to run test
-- 1) cabal test; or
-- 2) stack test

-- to run a specific spec
-- cabal test --test-options="-m Lang.Simp.Syntax.LexerSpec" 
-- or 
-- stack build :simp-test --ta "-m Lang.Simp.Syntax.LexerSpec"