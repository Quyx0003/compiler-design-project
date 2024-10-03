module Lang.Simp.Interpreter.PAInt where 

import Prelude hiding (lookup)
import qualified Data.Map as DM
import Data.List (sortOn)
import Control.Monad
import Lang.Simp.IR.PseudoAssembly 

-- | the Pseudo Assembly program interpretor by implementing the big step operational semantics

type PAProg = DM.Map Label LabeledInstr
type ErrMsg = String 


-- | The L environment. 
--  Though we define it as a mapping from Opr to Int, it is actually a mapping from Register or TempVar to Int
--  In the construction (see below) of L environment, we don't add LitInt into the domain of L environment.
type LEnv = DM.Map LKey Int 


data LKey = TVKey String 
    | RKey String 
    deriving (Show, Eq, Ord)

-- | update the local environment with the opr with a new value.
update :: Opr -> Int -> LEnv -> LEnv 
update (IntLit _) _ env         = env -- no change 
update (Regstr n) v env         = DM.insert (RKey n) v env
update (Temp (AVar n)) v env    = DM.insert (TVKey n) v env 


-- | lookup the local environment to search for the value associated with the opr 
lookup :: Opr -> LEnv -> Either ErrMsg Int 
lookup (IntLit v) env       = Right v
lookup (Regstr n) env       = 
    case DM.lookup (RKey n) env of 
        Nothing -> Left ("error: trying to access an uninitialized register " ++ n ++ ".")
        Just v  -> Right v 
lookup (Temp (AVar n)) env  = 
    case DM.lookup (TVKey n) env of 
        Nothing -> Left ("error: trying to access an uninitialized temp var " ++ n ++ ".")
        Just v  -> Right v 

-- | small step operational semantics of PA, execute the current labeled instruction by 1 step.
evalPA :: PAProg -> LEnv -> LabeledInstr -> Either ErrMsg (LEnv, LabeledInstr) 
-- (pConst), (pRegister), (pTempVar)
evalPA p env (lbl, IMove (IntLit _) _)      = Left "error: trying to move a value into a literal."
evalPA p env (lbl, IMove dst src)           = do 
    i    <- lookup src env
    let env' = update dst i env 
        nextLbl = lbl + 1
    nextLi <- case DM.lookup nextLbl p of
        Nothing -> Left ("error: invalid label " ++ show nextLbl ++ ".")
        Just v  -> Right v
    return (env', nextLi)   
-- (pOp)
evalPA p env (lbl, IPlus (IntLit _) _ _)    = Left "error: trying to move a value into a literal."
evalPA p env (lbl, IPlus dst src1 src2)     = do 
    i1 <- lookup src1 env
    i2 <- lookup src2 env 
    let env' = update dst (i1+i2) env 
        nextLbl = lbl + 1
    nextLi <- case DM.lookup nextLbl p of
        Nothing -> Left ("error: invalid label " ++ show nextLbl ++ ".")
        Just v  -> Right v
    return (env', nextLi)
evalPA p env (lbl, IMinus (IntLit _) _ _)    = Left "error: trying to move a value into a literal."
evalPA p env (lbl, IMinus dst src1 src2)     = do 
    i1 <- lookup src1 env
    i2 <- lookup src2 env 
    let env' = update dst (i1-i2) env 
        nextLbl = lbl + 1
    nextLi <- case DM.lookup nextLbl p of
        Nothing -> Left ("error: invalid label " ++ show nextLbl ++ ".")
        Just v  -> Right v
    return (env', nextLi)
evalPA p env (lbl, IMult (IntLit _) _ _)    = Left "error: trying to move a value into a literal."
evalPA p env (lbl, IMult dst src1 src2)     = do 
    i1 <- lookup src1 env
    i2 <- lookup src2 env 
    let env' = update dst (i1*i2) env 
        nextLbl = lbl + 1
    nextLi <- case DM.lookup nextLbl p of
        Nothing -> Left ("error: invalid label " ++ show nextLbl ++ ".")
        Just v  -> Right v
    return (env', nextLi)
evalPA p env (lbl, IDEqual (IntLit _) _ _)    = Left "error: trying to move a value into a literal."
evalPA p env (lbl, IDEqual dst src1 src2)     = do 
    i1 <- lookup src1 env
    i2 <- lookup src2 env 
    let env' = update dst (if i1 == i2 then 1 else 0) env 
        nextLbl = lbl + 1
    nextLi <- case DM.lookup nextLbl p of
        Nothing -> Left ("error: invalid label " ++ show nextLbl ++ ".")
        Just v  -> Right v
    return (env', nextLi)
evalPA p env (lbl, ILThan (IntLit _) _ _)    = Left "error: trying to move a value into a literal."
evalPA p env (lbl, ILThan dst src1 src2)     = do 
    i1 <- lookup src1 env
    i2 <- lookup src2 env 
    let env' = update dst (if i1 < i2 then 1 else 0) env 
        nextLbl = lbl + 1
    nextLi <- case DM.lookup nextLbl p of
        Nothing -> Left ("error: invalid label " ++ show nextLbl ++ ".")
        Just v  -> Right v
    return (env', nextLi)
-- (pIfn0) and (pIfnNot0)
evalPA p env (lbl, IIfNot cond lbl')         = do 
    bv <- lookup cond env 
    if bv == 0
    then case DM.lookup lbl' p of 
        Nothing  -> Left ("error: invalid label " ++ show lbl' ++ ".")
        Just li' -> Right (env, li')
    else 
        let lbl'' = lbl + 1
        in case DM.lookup lbl'' p of 
            Nothing  -> Left ("error: invalid label " ++ show lbl'' ++ ".")
            Just li'' -> Right (env, li'')
-- (pGoto)
evalPA p env (lbl, IGoto lbl')              = 
    case DM.lookup lbl' p of 
        Nothing -> Left ("error: invalid label " ++ show lbl' ++ ".")
        Just v  -> Right (env,v)
-- IRet, should not be used
evalPA p env (lbl, IRet) = Right (env, (lbl, IRet))

-- | keep running the under p until li reaches the IRet instruction
runUntilRet :: PAProg -> LEnv -> LabeledInstr -> Either ErrMsg (LEnv, LabeledInstr)
runUntilRet p env li = do 
    (env', li') <- evalPA p env li 
    case li' of 
        (l, IRet) -> Right (env', li')
        _         -> runUntilRet p env' li'

-- | Top level interpret function for a PA program.
interpret :: [LabeledInstr] -> Int -> Either ErrMsg Int 
interpret lis input = 
    let p = foldl (\m (l,i) -> DM.insert l (l,i) m) DM.empty lis
        env = DM.singleton (TVKey "input") input 
    in case getEntry lis of 
        Nothing -> Left "error: An empty PA program is given."
        Just li -> do 
            (env', _) <- runUntilRet p env li  
            case DM.lookup (RKey "_r_ret") env' of
                Nothing -> Left "error: trying to access an uninitialized register _r_ret"
                Just v  -> Right v

-- | get the entry labeled instruction from a list of labeled instructions
getEntry :: [LabeledInstr] -> Maybe LabeledInstr
getEntry [] = Nothing 
getEntry lis = Just (head (sortOn fst lis))
