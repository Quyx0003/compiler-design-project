module Lang.Simp.IR.Util where

import Control.Monad.State
import Lang.Simp.IR.PseudoAssembly
import Lang.Simp.Syntax.AST

data StateInfo = StateInfo {
    nextNum::Int,
    prefix::String,
    nextLbl::Int
} deriving Show


-- | the `newLabel` generates a new label from the state
newLabel :: State StateInfo Int
newLabel = do
    st <- get
    put st{nextLbl = 1 + nextLbl st}
    return (nextLbl st)


-- | the `newName` generates a new label from the state
newName :: State StateInfo String
newName = do
    st <- get
    put st{nextNum = 1 + nextNum st}
    return $ prefix st ++ "_" ++ show (nextNum st)


-- | the `newTemp` creates a new temporary variable
newTemp :: State StateInfo Opr 
newTemp = do 
    n <- newName 
    return (Temp (AVar n))


-- | the `chkNextLabel` returns the next label value without incrementing it
chkNextLabel :: State StateInfo Int 
chkNextLabel = do 
    st <- get
    return (nextLbl st)


-- | the `mkRegstr` creates a new register given name 
mkRegstr :: String -> Opr 
mkRegstr name = Regstr name 


-- | the function `var2AVar` converts a variable to an AVar
var2AVar :: Var -> AVar 
var2AVar (Var n) = AVar n 
