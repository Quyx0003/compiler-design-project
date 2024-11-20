module Lang.Simp.Lattice.SignLattice where 

import Lang.Simp.Lattice.CompleteLattice

data SignAbsVal = Bot   -- ^ _|_
    | Minus             -- ^ -
    | Plus              -- ^ +
    | Top               -- ^ T
    | Zero              -- ^ 0
    deriving (Show, Eq, Ord)

-- Cohort Problem 10 Exercise 2
instance CompleteLattice SignAbsVal where 
    sqSubsetEq Bot _ = True
    sqSubsetEq _ Top = True
    sqSubsetEq Minus Minus = True
    sqSubsetEq Plus Plus = True
    sqSubsetEq Zero Zero = True
    sqSubsetEq _ _ = False

    lub Bot x = x
    lub x Bot = x
    lub Top _ = Top
    lub _ Top = Top
    lub Minus Minus = Minus
    lub Plus Plus = Plus
    lub Zero Zero = Zero
    lub _ _ = Top
-- Cohort Problem 10 Exercise 2 End