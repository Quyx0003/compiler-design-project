{-# LANGUAGE MultiParamTypeClasses #-}
module Lang.Simp.Syntax.Lexer where

import Lang.Simp.Syntax.SrcLoc
import Lang.Simp.Syntax.Parsec hiding (tokens)
import Prelude hiding (getLine)
import Data.Char

-- | The `LEnv` data type defines a lexer environment to be used in Parsec 
data LEnv =
    LEnv { tokens::[Char]
    , ln :: Int
    , cl :: Int
    }
    deriving Show


-- | The `eof` function checks whether the given lexer env reaches the end of the input. 
eof :: LEnv -> Bool
eof (LEnv [] _ _) = True
eof _ = False

instance ParserEnv LEnv Char where
    getTokens = tokens
    getCol = cl
    getLine = ln
    setTokens ts lenv = lenv{tokens=ts}
    setLine l lenv = lenv{ln=l}
    setCol c lenv = lenv{cl=c}
    isNextTokNewLine lenv =
        case getTokens lenv of
            (c:_) | c == '\n' -> True
            _ -> False


whitespaces = ['\t', '\r', '\n', ' ', '\f']

type Id = String

-- | The `LToken` datatype defines the set of possible Lexical tokens
data LToken = EqSign SrcLoc  -- ^ = 
    | DEqSign SrcLoc         -- ^ ==
    | PlusSign SrcLoc        -- ^ + 
    | MinusSign SrcLoc       -- ^ - 
    | AsterixSign SrcLoc     -- ^ *
    | FSlashSign SrcLoc      -- ^ /

    | LThanSign SrcLoc       -- ^ <
    -- | GThanSign SrcLoc    -- ^ >

    | LBrace SrcLoc          -- ^ {
    | RBrace SrcLoc          -- ^ }
    | LParen SrcLoc          -- ^ (
    | RParen SrcLoc          -- ^ ) 
    | SemiColon SrcLoc       -- ^ ; 

    | RetKW SrcLoc           -- ^ return 
    | IfKW SrcLoc            -- ^ if 
    | ElseKW SrcLoc          -- ^ else
    | WhileKW SrcLoc         -- ^ while
    | NopKW SrcLoc           -- ^ nop
    | TrueKW SrcLoc          -- ^ true
    | FalseKW SrcLoc         -- ^ false

    | IdTok SrcLoc Id        -- ^ an identifier
    | IntTok SrcLoc Int      -- ^ an integer
    | WhiteSpace SrcLoc Char -- ^ a whitespace 

    deriving (Show, Eq)

srcLoc :: LToken -> SrcLoc
srcLoc (EqSign s) = s
srcLoc (DEqSign s) = s
srcLoc (PlusSign s) = s
srcLoc (MinusSign s) = s
srcLoc (AsterixSign s) = s
srcLoc (FSlashSign s) = s
srcLoc (LThanSign s) = s
-- srcLoc (GThanSign s) = s 
srcLoc (LBrace s) = s
srcLoc (RBrace s) = s
srcLoc (LParen s) = s
srcLoc (RParen s) = s
srcLoc (SemiColon s) = s
srcLoc (RetKW s) = s
srcLoc (IfKW s) = s
srcLoc (ElseKW s) = s
srcLoc (WhileKW s) = s
srcLoc (NopKW s) = s
srcLoc (TrueKW s) = s
srcLoc (FalseKW s) = s
srcLoc (IdTok s _) = s
srcLoc (IntTok s _) = s
srcLoc (WhiteSpace s _) = s

lEqSign :: Parser LEnv LToken
lEqSign = do
    { c <- sat ('=' ==) "expecting a '='"
    ; ln <- get getLine
    ; cl <- get getCol
    ; return (EqSign (SrcLoc ln cl))
    }

lDEqSign :: Parser LEnv LToken
lDEqSign = do
    { e1 <- sat ('=' ==) "expecting a '='"
    ; e2 <- sat ('=' ==) "expecting a '='"
    ; ln <- get getLine
    ; cl <- get getCol
    ; return (DEqSign (SrcLoc ln cl))
    }

lPlusSign :: Parser LEnv LToken
lPlusSign = do
    { c <- sat ('+' ==) "expecting a '+'"
    ; ln <- get getLine
    ; cl <- get getCol
    ; return (PlusSign (SrcLoc ln cl))
    }

lMinusSign :: Parser LEnv LToken
lMinusSign = do
    { c <- sat ('-' ==) "expecting a '-'"
    ; ln <- get getLine
    ; cl <- get getCol
    ; return (MinusSign (SrcLoc ln cl))
    }

lAsterixSign :: Parser LEnv LToken
lAsterixSign = do
    { c <- sat ('*' ==) "expecting a '*'"
    ; ln <- get getLine
    ; cl <- get getCol
    ; return (AsterixSign (SrcLoc ln cl))
    }


lFSlashSign :: Parser LEnv LToken
lFSlashSign = do
    { c <- sat ('/' ==) "expecting a '/'"
    ; ln <- get getLine
    ; cl <- get getCol
    ; return (FSlashSign (SrcLoc ln cl))
    }

lLThanSign :: Parser LEnv LToken
lLThanSign = do
    { c <- sat ('<' ==) "expecting a '<'"
    ; ln <- get getLine
    ; cl <- get getCol
    ; return (LThanSign (SrcLoc ln cl))
    }

lLBrace :: Parser LEnv LToken
lLBrace = do
    { c <- sat ('{' ==) "expecting a '{'"
    ; ln <- get getLine
    ; cl <- get getCol
    ; return (LBrace (SrcLoc ln cl))
    }


lRBrace :: Parser LEnv LToken
lRBrace = do
    { c <- sat ('}' ==) "expecting a '}'"
    ; ln <- get getLine
    ; cl <- get getCol
    ; return (RBrace (SrcLoc ln cl))
    }



lLParen :: Parser LEnv LToken
lLParen = do
    { c <- sat ('(' ==) "expecting a '('"
    ; ln <- get getLine
    ; cl <- get getCol
    ; return (LParen (SrcLoc ln cl))
    }


lRParen :: Parser LEnv LToken
lRParen = do
    { c <- sat (')' ==) "expecting a ')'"
    ; ln <- get getLine
    ; cl <- get getCol
    ; return (RParen (SrcLoc ln cl))
    }


lSemiColon :: Parser LEnv LToken
lSemiColon = do
    { c <- sat (';' ==) "expecting a ';'"
    ; ln <- get getLine
    ; cl <- get getCol
    ; return (SemiColon (SrcLoc ln cl))
    }

-- | The `string` function lexes the given string 
string :: String -> Error -> Parser LEnv String
string str errMsg  = Parser (\env ->
    case run (stringGo str) env of
        { Empty (Ok v) -> Empty (Ok v)
        ; Empty (Failed err) -> Empty (Failed errMsg)
        ; Consumed (Ok v) -> Consumed (Ok v)
        ; Consumed (Failed err) -> Consumed (Failed errMsg)
        }
    )

stringGo :: String -> Parser LEnv String
stringGo [] = return []
stringGo (x:xs) = do
    { t <- sat (x ==) ("expecting a " ++ [x])
    ; ts <- stringGo xs
    ; return (t:ts)
    }

-- | The `lRetKW` function lexes a `return` keyword 
lRetKW :: Parser LEnv LToken
lRetKW = do
    { cs <- string "return" "'return' is expected"
    ; ln <- get getLine
    ; cl <- get getCol
    ; return (RetKW (SrcLoc ln cl))
    }


-- | The `lIfKW` function lexes a `if` keyword 
lIfKW :: Parser LEnv LToken
lIfKW = do
    { cs <- string "if" "'if' is expected"
    ; ln <- get getLine
    ; cl <- get getCol
    ; return (IfKW (SrcLoc ln cl))
    }

-- | The `lElseKW` function lexes a `else` keyword 
lElseKW :: Parser LEnv LToken
lElseKW = do
    { cs <- string "else" "'else' is expected"
    ; ln <- get getLine
    ; cl <- get getCol
    ; return (ElseKW (SrcLoc ln cl))
    }


-- | The `lWhileKW` function lexes a `while` keyword 
lWhileKW :: Parser LEnv LToken
lWhileKW = do
    { cs <- string "while" "'while' is expected"
    ; ln <- get getLine
    ; cl <- get getCol
    ; return (WhileKW (SrcLoc ln cl))
    }

-- | The `lNopKW` function lexes a `nop` keyword 
lNopKW :: Parser LEnv LToken
lNopKW = do
    { cs <- string "nop" "'nop' is expected"
    ; ln <- get getLine
    ; cl <- get getCol
    ; return (NopKW (SrcLoc ln cl))
    }


-- | The `lTrueKW` function lexes a `true` keyword 
lTrueKW :: Parser LEnv LToken
lTrueKW = do
    { cs <- string "true" "'true' is expected"
    ; ln <- get getLine
    ; cl <- get getCol
    ; return (TrueKW (SrcLoc ln cl))
    }

-- | The `lFalseKW` function lexes a `false` keyword 
lFalseKW :: Parser LEnv LToken
lFalseKW = do
    { cs <- string "false" "'false' is expected"
    ; ln <- get getLine
    ; cl <- get getCol
    ; return (FalseKW (SrcLoc ln cl))
    }



-- | The `lIntTok` function lexes an integer 
lIntTok :: Parser LEnv LToken
lIntTok = do
    { _ <- lookAhead (sat isDigit "expecting a digit")
    ; cs <- everythingUntil (not . isDigit)
    ; ln <- get getLine
    ; cl <- get getCol
    ; return (IntTok (SrcLoc ln cl) (read cs))
    }

-- | The `lIdTok` function lexes an identifier 
lIdTok :: Parser LEnv LToken
lIdTok = do
    { _ <- lookAhead (sat isAlpha "expecting a alphabet")
    ; cs <- everythingUntil (not . (\c -> isAlpha c || isDigit c || c == '_' ))
    ; ln <- get getLine
    ; cl <- get getCol
    ; return (IdTok (SrcLoc ln cl) cs)
    }

-- | The `lWhiteSpace` function lexes a white space.
lWhiteSpace :: Parser LEnv LToken
lWhiteSpace = do
    { c <- sat (`elem` whitespaces) "expecting a whitespace"
    ; ln <- get getLine
    ; cl <- get getCol
    ; return (WhiteSpace (SrcLoc ln cl) c)
    }


-- | The `lexOne` function applies lexing to one token 
lexOne :: Parser LEnv  LToken 
lexOne = choices 
    [ attempt lDEqSign
    , lEqSign
    , lPlusSign
    , lMinusSign
    , lAsterixSign 
    , lFSlashSign
    , lLThanSign 
    , lLBrace
    , lRBrace
    , lLParen
    , lRParen
    , lSemiColon
    , attempt lRetKW
    , attempt lIfKW 
    , attempt lElseKW 
    , attempt lWhileKW 
    , attempt lTrueKW
    , attempt lFalseKW
    , lIntTok
    , lIdTok
    ] lWhiteSpace

-- | The `lex` is the top level lexer function 
lex :: Parser LEnv [LToken] 
lex = many lexOne