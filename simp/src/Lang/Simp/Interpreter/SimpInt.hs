module Lang.Simp.Interpreter.SimpInt where

import Prelude hiding (lookup)
import qualified Data.Map as DM
import Data.List (sortOn)
import Control.Monad
import Lang.Simp.Syntax.AST

-- | the Simp program interpretor by implementing the big step operational semantics

-- | the variable environment
type Delta = DM.Map Var Const

type ErrMsg = String

-- | implementing the big step operational semantics Delta, e \Downarrow c
-- the rules are partial function, hence we need an Either ErrMsg monad
evalExp :: Delta -> Exp -> Either ErrMsg Const
evalExp dlt (ConstExp l)    = Right l
evalExp dlt (DEqual e1 e2)  = do
    c1 <- evalExp dlt e1
    c2 <- evalExp dlt e2
    eqConst c1 c2
evalExp dlt (LThan e1 e2)   = do
    c1 <- evalExp dlt e1
    c2 <- evalExp dlt e2
    ltConst c1 c2
evalExp dlt (Minus e1 e2)   = do
    c1 <- evalExp dlt e1
    c2 <- evalExp dlt e2
    minusConst c1 c2
evalExp dlt (Mult e1 e2)    = do
    c1 <- evalExp dlt e1
    c2 <- evalExp dlt e2
    multConst c1 c2
evalExp dlt (Plus e1 e2)    = do
    c1 <- evalExp dlt e1
    c2 <- evalExp dlt e2
    plusConst c1 c2
-- Lab 2 Task 1.1 
evalExp dlt (VarExp v)      = undefined -- fixme
evalExp dlt (ParenExp e)    = undefined -- fixme
-- Lab 2 Task 1.1 end


-- | implementing the big step operational semantics Delta, s \Downarrow Delta'
-- the rules are partial function, hence we need an Either ErrMsg monad
class Evaluable a where
    eval :: Delta -> a -> Either ErrMsg Delta


instance Evaluable a => Evaluable [a] where
    eval dlt [] = Right dlt
    -- Lab 2 Task 1.2 
    eval dlt (x:xs) = undefined -- fixme 
    -- Lab 2 Task 1.2 end


instance Evaluable Stmt where
    eval dlt Nop             = Right dlt
    eval dlt (Assign x e)    = do
        c <- evalExp dlt e
        return (DM.insert x c dlt)
    eval dlt (If cond th el) = do
        c <- evalExp dlt cond
        case c of
            IntConst _      -> Left "int expression found in the if condition position."
            BoolConst b
                | b         -> eval dlt th
                | otherwise -> eval dlt el
    eval dlt (Ret x)        = Right dlt
    -- Lab 2 Task 1.2 
    eval dlt (While cond s) = undefined -- fixme
    -- Lab 2 Task 1.2 end 


eqConst :: Const -> Const -> Either ErrMsg Const
eqConst (IntConst i1) (IntConst i2)     = Right (BoolConst (i1 == i2))
eqConst (BoolConst b1) (BoolConst b2)   = Right (BoolConst (b1 == b2))
eqConst _ _                             = Left "different types of values are compared using =="


ltConst :: Const -> Const -> Either ErrMsg Const
ltConst (IntConst i1) (IntConst i2)     = Right (BoolConst (i1 < i2))
ltConst _ _                             = Left "non int type of values are compared using <"

minusConst :: Const -> Const -> Either ErrMsg Const
minusConst (IntConst i1) (IntConst i2)     = Right (IntConst (i1 - i2))
minusConst _ _                             = Left "non int type of values are used with -"

multConst :: Const -> Const -> Either ErrMsg Const
multConst (IntConst i1) (IntConst i2)     = Right (IntConst (i1 * i2))
multConst _ _                             = Left "non int type of values are used with *"

plusConst :: Const -> Const -> Either ErrMsg Const
plusConst (IntConst i1) (IntConst i2)     = Right (IntConst (i1 + i2))
plusConst _ _                             = Left "non int type of values are used with +"


-- | The top level interpretor function for SIMP program

interpret :: [Stmt] -> Int -> Either ErrMsg Const
interpret p input = do
    let dlt = DM.singleton (Var "input") (IntConst input)
    v <- getLastRetVar p
    dlt' <- eval dlt p
    case DM.lookup v dlt' of
        Nothing -> Left ("undefined variable " ++ varname v ++ ".")
        Just  v -> Right v


getLastRetVar :: [Stmt] -> Either ErrMsg Var
getLastRetVar [] = Left "error: the program is empty"
getLastRetVar stmts = case last stmts of
    Ret x -> Right x
    _     -> Left "error: the last statement of the SIMP program is not a return."
