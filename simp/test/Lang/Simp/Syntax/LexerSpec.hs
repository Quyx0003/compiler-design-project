module Lang.Simp.Syntax.LexerSpec where 

import Prelude hiding (lex)
import Test.Hspec
import Lang.Simp.Syntax.Parsec
import Lang.Simp.Syntax.SrcLoc
import Lang.Simp.Syntax.Lexer 

spec :: Spec
spec = do
    describe "lexer" $ do
            
        it "lexing y = 1; x = x + 1;" $ 
            let src = "y = 1; x = x + 1;"
                expected = [ IdTok (SrcLoc 1 2) "y", WhiteSpace (SrcLoc 1 3) ' ', 
                    EqSign (SrcLoc 1 4), WhiteSpace (SrcLoc 1 5) ' ', IntTok (SrcLoc 1 6) 1, 
                    SemiColon (SrcLoc 1 7), WhiteSpace (SrcLoc 1 8) ' ', IdTok (SrcLoc 1 9) "x", WhiteSpace (SrcLoc 1 10) ' ', 
                    EqSign (SrcLoc 1 11), WhiteSpace (SrcLoc 1 12) ' ', IdTok (SrcLoc 1 13) "x", WhiteSpace (SrcLoc 1 14) ' ', 
                    PlusSign (SrcLoc 1 15), WhiteSpace (SrcLoc 1 16) ' ', IntTok (SrcLoc 1 17) 1, SemiColon (SrcLoc 1 18) ]
                lenv = LEnv src 1 1
            in case run lex lenv of 
                { Consumed (Ok (toks, lenv)) | eof lenv  -> toks `shouldBe` expected 
                                             | otherwise -> expectationFailure "lexing failed, the remaining token stream is not empty."
                ; Consumed (Failed err)                  -> expectationFailure err
                ; Empty (Ok (toks, lenv)) | eof lenv     -> toks `shouldBe` expected
                                          | otherwise    -> expectationFailure "lexing failed, no token is lexed, the remaining token stream is not empty."
                ; Empty (Failed err)                     -> expectationFailure err
                } 
        
        it "lexing y = input; " $ 
            let src = "y = input;"
                expected = [ IdTok (SrcLoc 1 2)  "y", WhiteSpace (SrcLoc 1 3) ' ', EqSign (SrcLoc 1 4), 
                    WhiteSpace (SrcLoc 1 5) ' ', IdTok (SrcLoc 1 10) "input", SemiColon (SrcLoc 1 11) ]
                lenv = LEnv src 1 1
            in case run lex lenv of 
                { Consumed (Ok (toks, lenv)) | eof lenv  -> toks `shouldBe` expected 
                                             | otherwise -> expectationFailure "lexing failed, the remaining token stream is not empty."
                ; Consumed (Failed err)                  -> expectationFailure err
                ; Empty (Ok (toks, lenv)) | eof lenv     -> toks `shouldBe` expected
                                          | otherwise    -> expectationFailure "lexing failed, no token is lexed, the remaining token stream is not empty."
                ; Empty (Failed err)                     -> expectationFailure err
                } 


        it "test lexer: lexing\ 
                \x = input; \
                \s = 0; \
                \c = 0; \
                \while c < x { \ 
                \   s = c + s; \
                \   c = c + 1; \
                \} \
                \return s;" $ 
            let src = "\nx = input;\n\
                    \s = 0;\n\
                    \c = 0;\n\
                    \while c < x {\n\ 
                    \    s = c + s;\n\
                    \    c = c + 1;\n\
                    \}\n\
                    \return s;"
                expected = [
                    WhiteSpace (SrcLoc 2 1) '\n', IdTok (SrcLoc 2 2) "x", WhiteSpace (SrcLoc 2 3) ' ', EqSign (SrcLoc 2 4), WhiteSpace (SrcLoc 2 5) ' ', IdTok (SrcLoc 2 10) "input", SemiColon (SrcLoc 2 11), 
                    WhiteSpace (SrcLoc 3 1) '\n', IdTok (SrcLoc 3 2) "s", WhiteSpace (SrcLoc 3 3) ' ', EqSign (SrcLoc 3 4), WhiteSpace (SrcLoc 3 5) ' ', IntTok (SrcLoc 3 6) 0, SemiColon (SrcLoc 3 7), 
                    WhiteSpace (SrcLoc 4 1) '\n', IdTok (SrcLoc 4 2) "c", WhiteSpace (SrcLoc 4 3) ' ', EqSign (SrcLoc 4 4), WhiteSpace (SrcLoc 4 5) ' ', IntTok (SrcLoc 4 6) 0, SemiColon (SrcLoc 4 7), 
                    WhiteSpace (SrcLoc 5 1) '\n', WhileKW (SrcLoc 5 6), WhiteSpace (SrcLoc 5 7) ' ', IdTok (SrcLoc 5 8) "c", WhiteSpace (SrcLoc 5 9) ' ', LThanSign (SrcLoc 5 10), WhiteSpace (SrcLoc 5 11) ' ', IdTok (SrcLoc 5 12) "x", WhiteSpace (SrcLoc 5 13) ' ', LBrace (SrcLoc 5 14), 
                    WhiteSpace (SrcLoc 6 1) '\n', WhiteSpace (SrcLoc 6 2) ' ', WhiteSpace (SrcLoc 6 3) ' ', WhiteSpace (SrcLoc 6 4) ' ', WhiteSpace (SrcLoc 6 5) ' ', IdTok (SrcLoc 6 6) "s", WhiteSpace (SrcLoc 6 7) ' ', EqSign (SrcLoc 6 8), WhiteSpace (SrcLoc 6 9) ' ', IdTok (SrcLoc 6 10) "c", WhiteSpace (SrcLoc 6 11) ' ', PlusSign (SrcLoc 6 12), WhiteSpace (SrcLoc 6 13) ' ', IdTok (SrcLoc 6 14) "s", SemiColon (SrcLoc 6 15), 
                    WhiteSpace (SrcLoc 7 1) '\n', WhiteSpace (SrcLoc 7 2) ' ', WhiteSpace (SrcLoc 7 3) ' ', WhiteSpace (SrcLoc 7 4) ' ', WhiteSpace (SrcLoc 7 5) ' ', IdTok (SrcLoc 7 6) "c", WhiteSpace (SrcLoc 7 7) ' ', EqSign (SrcLoc 7 8), WhiteSpace (SrcLoc 7 9) ' ', IdTok (SrcLoc 7 10) "c", WhiteSpace (SrcLoc 7 11) ' ', PlusSign (SrcLoc 7 12), WhiteSpace (SrcLoc 7 13) ' ', IntTok (SrcLoc 7 14) 1, SemiColon (SrcLoc 7 15), 
                    WhiteSpace (SrcLoc 8 1) '\n', RBrace (SrcLoc 8 2), 
                    WhiteSpace (SrcLoc 9 1) '\n', RetKW (SrcLoc 9 7), WhiteSpace (SrcLoc 9 8) ' ', IdTok (SrcLoc 9 9) "s", SemiColon (SrcLoc 9 10)]
                lenv = LEnv src 1 1
            in case run lex lenv of 
                { Consumed (Ok (toks, lenv)) | eof lenv  -> toks `shouldBe` expected 
                                             | otherwise -> expectationFailure "lexing failed, the remaining token stream is not empty."
                ; Consumed (Failed err)                  -> expectationFailure err
                ; Empty (Ok (toks, lenv)) | eof lenv     -> toks `shouldBe` expected
                                          | otherwise    -> expectationFailure "lexing failed, no token is lexed, the remaining token stream is not empty."
                ; Empty (Failed err)                     -> expectationFailure err
                } 
