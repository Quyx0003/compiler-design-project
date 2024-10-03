module Lang.Simp.Syntax.AST where

-- | The Statement data type 
data Stmt = Assign Var Exp -- ^ The constructor for an Assignment statement 
    | If Exp [Stmt] [Stmt] -- ^ The constructor for an If-else statement 
    | Nop                  -- ^ The constructor for a NOP statement 
    | While Exp [Stmt]     -- ^ The constructor for a While statement
    | Ret Var              -- ^ The consturctor for a Return statement
    deriving (Show, Eq) 


newtype Var = Var String deriving (Show, Eq, Ord) 

-- | The function `varname` returns the string repr of the variable name.  
varname :: Var -> String 
varname (Var n) = n 


-- | The Expression data type
data Exp = Plus Exp Exp -- ^ The constructor for a plus expression e1 + e2
    | Minus Exp Exp     -- ^ The constructor for a subtraction expression e1 - e2          
    | Mult Exp Exp      -- ^ The constructor for a multiplication expression e1 * e2
    | DEqual Exp Exp    -- ^ The constructor for an equality test expression e1 == e2
    | LThan Exp Exp     -- ^ The constructor for a less-than test expression e1 < e2
    | ConstExp Const    -- ^ The constructor for a constant expression, e.g. 1, True 
    | VarExp Var        -- ^ The constructor for a variable expression, e.g. x, foo 
    | ParenExp Exp      -- ^ The constructor for a parenthese expression, ( e )
    deriving (Show, Eq)

-- | The Constant data type 
data Const = IntConst Int   -- ^ The constructor for an integer constant 
    | BoolConst Bool        -- ^ The constructor for a boolean constant
    deriving (Show, Eq) 