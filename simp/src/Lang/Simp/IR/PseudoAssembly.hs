module Lang.Simp.IR.PseudoAssembly where


{-
(Labeled Instr) li ::= l : i
(Instruction) i ::= d <- s | d <- s op s | ret | ifn s goto l | goto l
(Operand)   d,s ::= r | c | t 
(Temp Var)    t ::= x | y | .. 
(Label)       l ::= 1 | 2 | ...
-}

type LabeledInstr = (Label, Instr)
type Label = Int 

data Instr = IMove Opr Opr
    | IPlus Opr Opr Opr
    | IMinus Opr Opr Opr
    | IMult Opr Opr Opr
    | IDEqual Opr Opr Opr
    | ILThan Opr Opr Opr
    | IRet
    | IIfNot Opr Label
    | IGoto Label
    deriving (Show, Eq)

data Opr = Regstr String
    | IntLit Int
    | Temp AVar
    deriving (Show, Eq, Ord) 


newtype AVar = AVar {name::String} deriving (Show, Eq, Ord) 



isIRet IRet = True
isIRet _ = False


vars :: Opr -> [String] 
vars (Temp (AVar n)) = [n] 
vars _ = [] 

allVars :: Instr -> [String]
allVars (IMove t s)         = vars t ++ vars s 
allVars (IPlus t s1 s2)     = vars t ++ vars s1 ++ vars s2
allVars (IMinus t s1 s2)    = vars t ++ vars s1 ++ vars s2
allVars (IMult t s1 s2)     = vars t ++ vars s1 ++ vars s2
allVars (IDEqual t s1 s2)   = vars t ++ vars s1 ++ vars s2
allVars (ILThan t s1 s2)    = vars t ++ vars s1 ++ vars s2
allVars IRet                = []
allVars (IIfNot t l)        = vars t 
allVars (IGoto _)           = [] 