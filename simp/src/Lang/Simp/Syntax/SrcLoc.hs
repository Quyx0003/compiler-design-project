module Lang.Simp.Syntax.SrcLoc where


data SrcLoc =
    -- | The constructor for a SrcLoc data type taking in line and column numbers 
    SrcLoc Int Int 
    deriving (Show, Eq)

-- | The `line` function returns the line number from a `SrcLoc` datatype 
line :: SrcLoc -> Int 
line (SrcLoc ln _) = ln 


-- | The `column` function returns the column number from a `SrcLoc` datatype
column :: SrcLoc -> Int 
column (SrcLoc _ col) = col 