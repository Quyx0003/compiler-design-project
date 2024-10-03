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
    sqSubsetEq = undefined -- fixme 
    lub = undefined -- fixme 
-- Cohort Problem 10 Exercise 2 End