{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
module Lang.Simp.Backend.Wasm where


import qualified Data.Map as DM
import Control.Monad 
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Debug.Trace (trace)
import Language.Wasm.Builder
import Language.Wasm.Structure
import Language.Wasm
import Data.ByteString

import qualified Data.Text.Lazy as DTL 

import Prelude hiding (writeFile, and)
import Lang.Simp.IR.PseudoAssembly
import Data.Proxy



{- 
(Wasm Instructions) wis ::= wi ; wis | [] 
(Wasm Instruction) wi   ::= pi  | bi 
(Plain Instruction) pi  ::= nop | br L | brIf L | return | get n | set n | const c | add | sub | mul | eq | lt 
(Block Instruction) bi  ::= block { wis } | loop { wis } | if { wis } else { wis }
(Level) L               ::= 0 | 1 | 2 | ... 
(Variables) n           ::= x | y | z | ...  


(Wasm Environment) \delta \subseteq n x c
(Value Stack)         s ::= _,_ | c,_ | c,c 
(Block Stack)         B :: [] | (bi,wis); B


Operattional Semantics   (\Delta, S , wis, B) --> (\Delta', S', wis', B') 

(tee)??

(set1)          (\Delta, _, _, set n; wis, B) --> (\Delta, \Delta(n), _, wis, B)

(set2)          (\Delta, c, _, set n; wis, B) --> (\Delta, c, \Delta(n), wis, B)

(get)           (\Delta, c, _, get n; wis; B) --> (\Delta \oplus (n,c), _, _, wis, B)

(const1)        (\Delta, _, _, const c; wis, B) --> (\Delta, c, _, wis, B)

(const2)        (\Delta, c0, _, const c1; wis, B) --> (\Delta, c0, c1, wis, B)

(add)           (\Delta, c0, c1, add; wis, B) --> (\Delta, c0+c1, _, wis, B) 

(sub)           (\Delta, c0, c1, sub; wis, B) --> (\Delta, c0-c1, _, wis, B) 

(mul)           (\Delta, c0, c1, mul; wis, B) --> (\Delta, c0*c1, _, wis, B) 

                                c0 == c1
(eq1)           ------------------------------------------------------------
                (\Delta, c0, c1, eq; wis, B) --> (\Delta, 1, _, wis, B)


                                c0 /= c1
(eq2)           ------------------------------------------------------------
                (\Delta, c0, c1, eq; wis, B) --> (\Delta, 0, _, wis, B)



                                c0 < c1
(lt1)           ------------------------------------------------------------
                (\Delta, c0, c1, lt; wis, B) --> (\Delta, 1, _, wis, B)


                                c0 >= c1
(lt2)           ------------------------------------------------------------
                (\Delta, c0, c1, lt; wis, B) --> (\Delta, 0, _, wis, B)



(br0Block)      (\Delta, r0, r1, br 0, (block {wis'}, wsi'');B ) --> (\Delta, r0, r1, wis'', B)

(br0Loop)       (\Delta, r0, r1, br 0, (loop {wis'}, wsi'');B ) --> (\Delta, r0, r1, wis', (loop {wis'}, wis'');B)

(brn)           (\Delta, r0, r1, br n, (bi, wis);B ) --> (\Delta, r0, r1, br (n-1), B)

(brIfTn)        (\Delta, 1, _, brIf n;wis, (bi, wsi');B ) --> (\Delta, 1, _, brIf (n-1);wis, B)

(brIfT0Block)   (\Delta, 1, _, brIf 0;wis, (block {wis'}, wis'');B ) --> (\Delta, _, _, wis'', B)

(brIfT0Loop)    (\Delta, 1, _, brIf 0;wis, (loop {wis'}, wis'');B ) --> (\Delta, _, _, wis', (loop {wis'}, wis'');B)

(brIfF)         (\Delta, 0, _, brIf n;wis, B) --> (\Delta, _, _, wis, B)

(block)         (\Delta, r0, r1, block {wis};wis', B) --> (\Delta, r0, r1, wis, (block {wis}, wis'); B)

(loop)          (\Delta, r0, r1, lopp {wis};wis', B) --> (\Delta, r0, r1, wis, (loop {wis}, wis'); B)

(ifT)           (\Delta, 1, _, if {wis} else {wis'};wis'', B) --> (\Delta, _, _, wis, (block {wis}, wis'');B)

(ifF)           (\Delta, 0, _, if {wis} else {wis'};wis'', B) --> (\Delta, _, _, wis', (block {wis'}, wis'');B)

(nop)           (\Delta, r0, r1, nop;wis, B ) --> (\Delta, r0, r1, wis, B )

(empty)         (\Delta, r0, r1, [], (bi, wis');B ) --> (\Delta, r0, r1, wis', B )


-}




{-
PA to Wasm conversion 

-}

-- | M is an environment mapping PA variable names to Wasm local variables
type M = DM.Map String (Loc I32)


-- | Generate the M environment and generate the variable declarations
generateMEnv :: [LabeledInstr] -> GenFun M 
generateMEnv lis = 
    let vars = Prelude.concatMap (\(l, i) -> allVars i) lis 
    in foldM go DM.empty vars 
    where 
        go :: M -> String -> GenFun M
        go m "input" = do 
            t <- param i32 
            return (DM.insert "input" t m)
        go m var     = do 
            t <- local i32
            return (DM.insert var t m)


            



convertInstr :: M -> [LabeledInstr] -> GenFun (Proxy I32)
convertInstr m [] = return Proxy
convertInstr m (li:lis) = case li of
    {-
            M |-_{src} s => wis1    
    (wReturn)-------------------------------------------
            M |- l : rret <- s; l2: ret; => [wis1] + [ get M(t)] + wis2
     in fold form
            M |-_{src} s => wis1     
    (wReturn)-------------------------------------------
            M |- l : rret <- s; l2: ret; => ret wis1
    -}
    (l, IMove (Regstr "_r_ret") s) | not (Prelude.null lis) && isIRet (snd $ Prelude.head lis) -> 
        ret (convertSrcOpr m s) 

    {-
            M |-_{src} s => wis1     M |- lis => wis2
    (wMove)-------------------------------------------
            M |- l : t <- s; lis => [wis1] + [ get M(t)] + wis2
     in fold form
            M |-_{src} s => wis1     M |- lis => wis2
    (wMove)-------------------------------------------
            M |- l : t <- s; lis => [ M(t) = wis1 ] + wis2
    -}
    (l, IMove t s) -> do
        convertDstOpr m t .= convertSrcOpr m s
        convertInstr m lis

    (l1, IDEqual t s1 s2) -> case lis of
        (l2, IIfNot t l3):lis' ->
            case lookup (l3-1) lis' of
                Nothing -> error $ "convertInstr failed: the labels " ++ show (l3-1) ++ " " ++ show l3 ++ " are not continuous"
                {-        
                        (l3-1):goto l4 \in lis'      l4 == l1
                        lis1, lis2 = split(l3, lis') 
                        M |-_{src} s1 => wis1    M |-_{src} s2 => wis2  
                        M |- lis1 => wis3        M|- lis2 => wis4
                (wEqLoop) -----------------------------------------------------------
                        M |- l1: t <- s1 == s2; l2 ; ifn t goto l3; lis' => 
                            wis1 + wis2 + [ eq, if { loop { wis3 + wis1 + wis2 + [ eq; brIf 0] } } else { nop } ] + wis4
                
                  in fold form       
                        (l3-1):goto l4 \in lis'      l4 == l1
                        lis1, lis2 = split(l3, lis') 
                        M |-_{src} s1 => wis1    M |-_{src} s2 => wis2  
                        M |- lis1 => wis3      M|- lis2 => wis4
                (wEqLoop) -----------------------------------------------------------
                        M |- l1: t <- s1 == s2; l2 ; ifn t goto l3; lis' => 
                            [ if (wis1 `eq` wis2) { loop { wis3;  wis1 `eq` wis 2; brIf 0 } } else { nop } ] + wis4
                -}
                Just (IGoto l4) | l4 == l1 -> case splitAtLbl l3 lis' of
                    (lis1, lis2) -> do
                        -- `while pred wis` is a short hand for   
                        -- `if pred { loop { wis;  pred ; brIf 0 } } else { nop }`
                        --  which is defined in Builder.hs
                        while (convertSrcOpr m s1 `eq` convertSrcOpr m s2) $ do
                            { convertInstr m lis1 ; return () }
                        convertInstr m lis2
                {-        
                        (l3-1):goto l4 \in lis'      l4 /= l1
                        lis1, lis2 = split(l3, lis') 
                        lis3, lis4 = split(l4, lis2)
                        M |-_{src} s1 => wis1    M |-_{src} s2 => wis2  
                        M |- lis1 => wis3        M |- lis3 => wis4   M |- lis4 => wis5
                (wEqIf) -----------------------------------------------------------
                        M |- l1: t <- s1 == s2; l2 ; ifn t goto l3; lis' => 
                            wis1 + wis2 + [ eq, if { wis3 } else { wis4 } ] + wis5
                
                 in fold form       
                        (l3-1):goto l4 \in lis'      l4 /= l1
                        lis1, lis2 = split(l3, lis') 
                        is3, lis4 = split(l4, lis2)
                        M |-_{src} s1 => wis1    M |-_{src} s2 => wis2  
                        M |- lis1 => wis3        M |- lis3 => wis4   M |- lis4 => wis5
                (wEqIf) -----------------------------------------------------------
                        M |- l1: t <- s1 == s2; l2 ; ifn t goto l3; lis' => 
                            [ if (wis1 `eq` wis2) { wis3 } else { wis4 } ] + wis5
                -}
                                | otherwise -> case splitAtLbl l3 lis' of
                    (lis1, lis2) -> case splitAtLbl l4 lis2 of
                        (lis3, lis4) -> do
                            if' () (convertSrcOpr m s1 `eq` convertSrcOpr m s2)
                                ( do { convertInstr m lis1 ; return () })
                                ( do { convertInstr m lis3 ; return () })
                            convertInstr m lis4
                Just _ -> error $ "convertInstr failed: the labeled instruction before the else branch " ++ show (l3-1) ++  "is not a goto."
                {-         
                        M |-_{src} s1 => wis1    M |-_{src} s2 => wis2  
                        M |- lis' => wis3        
                        head(lis') is not an ifn instruction.
                (wEq) -----------------------------------------------------------
                        M |- l: t <- s1 == s2; lis' =>  wis1 + wis2 + [ eq ] + wis3
                
                in fold form       
                        M |-_{src} s1 => wis1    M |-_{src} s2 => wis2  
                        M |- lis => wis3        
                        head(lis) is not an ifn instruction.
                (wEq) -----------------------------------------------------------
                        M |- l: t <- s1 == s2; lis =>  wis1 `eq` wis2 + wis3                -}
        _ -> do
            convertSrcOpr m s1 `eq` convertSrcOpr m s2
            convertInstr m lis

        

    (l1, ILThan t s1 s2) -> case lis of
        (l2, IIfNot t l3):lis' ->
            case lookup (l3 - 1) lis' of
                Nothing -> error $ "convertInstr failed: the labels " ++ show (l3 - 1) ++ " " ++ show l3 ++ " are not continuous"

                -- Lab 3 Task 3
                -- Handle the loop case where `l4 == l1`
                Just (IGoto l4) | l4 == l1 -> case splitAtLbl l3 lis' of
                    (lis1, lis2) -> do
                        -- Emit a `while` loop
                        while (convertSrcOpr m s1 `lt_s` convertSrcOpr m s2) $ do
                            void $ convertInstr m lis1
                        convertInstr m lis2

                -- Handle the if-else case where `l4 /= l1`
                Just (IGoto l4) -> case splitAtLbl l3 lis' of
                    (lis1, lis2) -> case splitAtLbl l4 lis2 of
                        (lis3, lis4) -> do
                            -- Emit an `if` block
                            if' () (convertSrcOpr m s1 `lt_s` convertSrcOpr m s2)
                                (do { convertInstr m lis1; return () })  -- Then branch
                                (do { convertInstr m lis3; return () }) -- Else branch
                            convertInstr m lis4

                -- Error if no valid `goto` instruction is found
                _ -> error $ "convertInstr failed: the labeled instruction before the else branch " ++ show (l3 - 1) ++ " is not a goto."

        -- Default behavior if the next instruction is not `IIfNot`
        _ -> do
            convertDstOpr m t .= convertSrcOpr m s1 `lt_s` convertSrcOpr m s2
            convertInstr m lis


    {-         
            M |-_{src} s1 => wis1    M |-_{src} s2 => wis2  
            M |- lis' => wis3        
            head(lis') is not a ifn instruction.
    (wPlus) -----------------------------------------------------------
            M |- l: t <- s1 + s2; lis' =>  wis1 + wis2 + [ add, get M(t) ] + wis3
    
    in fold form       
            M |-_{src} s1 => wis1    M |-_{src} s2 => wis2  
            M |- lis => wis3        
            head(lis) is not a ifn instruction.
    (wPlus) -----------------------------------------------------------
            M |- l: t <- s1 + s2; lis =>  M(t) = wis1 `add` wis2 + wis3    
    -}
    (l, IPlus t s1 s2) -> do 
        convertDstOpr m t .= convertSrcOpr m s1 `add` convertSrcOpr m s2
        convertInstr m lis
    
    {-         
            M |-_{src} s1 => wis1    M |-_{src} s2 => wis2  
            M |- lis' => wis3        
            head(lis') is not a ifn instruction.
    (wMinus) -----------------------------------------------------------
            M |- l: t <- s1 - s2; lis' =>  wis1 + wis2 + [ sub, get M(t) ] + wis3
    
    in fold form       
            M |-_{src} s1 => wis1    M |-_{src} s2 => wis2  
            M |- lis => wis3        
            head(lis) is not a ifn instruction.
    (wMinus) -----------------------------------------------------------
            M |- l: t <- s1 - s2; lis =>  M(t) =  wis1 `sub` wis2 + wis3    
    -}
    (l, IMinus t s1 s2) -> do 
        convertDstOpr m t .= convertSrcOpr m s1 `sub` convertSrcOpr m s2
        convertInstr m lis


    {-         
            M |-_{src} s1 => wis1    M |-_{src} s2 => wis2  
            M |- lis' => wis3        
            head(lis') is not a ifn instruction.
    (wMult) -----------------------------------------------------------
            M |- l: t <- s1 * s2; lis' =>  wis1 + wis2 + [ mul, get M(t) ] + wis3
    
    in fold form       
            M |-_{src} s1 => wis1    M |-_{src} s2 => wis2  
            M |- lis => wis3        
            head(lis) is not a ifn instruction.
    (wMult) -----------------------------------------------------------
            M |- l: t <- s1 * s2; lis =>  M(t) = wis1 `mul` wis2 + wis3    
    -}
    (l, IMult t s1 s2) -> do 
        convertDstOpr m t .= convertSrcOpr m s1 `mul` convertSrcOpr m s2
        convertInstr m lis    

    {-
             M |- lis => wis
    (wGoto) ------------------------------------------
            M |- l: goto l'; lis => wis 
    -}
    (l, IGoto _) -> do 
        convertInstr m lis
    

-- | split the list of ordered labeled instruction by label.
--  first of the result pair contains those with labels < lbl 
--  second of the result contains those with labels >= lbl
splitAtLbl :: Lang.Simp.IR.PseudoAssembly.Label -> [LabeledInstr] -> ([LabeledInstr], [LabeledInstr])
splitAtLbl lbl = Prelude.span (\(l,i) -> l /= lbl)






-- | convert a source operand into a sequence of wasm instructions
-- |  that read the value of the source operand and insert it into the value stack
-- it is going through the GenFun monad because it generate codes that updates the value stack 
convertSrcOpr ::  M -> Opr -> GenFun (Proxy I32)
--  M |-_{src} c => const c 
convertSrcOpr m (IntLit c) = i32c c
--  M |-_{src} t => set M(t)
convertSrcOpr m (Temp (AVar n)) = produce (m DM.! n)
convertSrcOpr m (Regstr n) = error $ "convertSrcOpr failed: register " ++ n ++ " should not appear as an operands in Pseudo Assembly."


-- | convert a destination PA operand into a Wasm local variable 
convertDstOpr :: M -> Opr -> Loc I32
convertDstOpr m (Temp (AVar t)) = m DM.! t
convertDstOpr m (Regstr n) = error $ "convertDstOpr failed: register " ++ n ++ " should not appear as an operands in Pseudo Assembly."



convertMod :: String -> [LabeledInstr] -> Module 
convertMod funcName lis = genMod $ do 
    wasmFunc <- fun i32 $ do
        m <- generateMEnv lis
        convertInstr m lis
    export (DTL.pack funcName) wasmFunc
    return ()


saveMod :: String -> Module ->  IO ()
saveMod wasmFile mod = do
    let binary = encode mod -- binary = encode emptyModule
    writeFile wasmFile binary
