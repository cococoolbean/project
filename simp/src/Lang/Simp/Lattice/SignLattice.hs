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
    sqSubsetEq s1 s2 
        | s1 == Bot          = True
        | s2 == Top          = True
        | s1 == s2           = True
        | otherwise          = False
    lub s1 s2          
        | s1 == Bot = s2
        | s2 == Bot = s1
        | s1 == Top || s2 == Top = Top
        | s1 == s2 = s1
        | otherwise = Top
-- Cohort Problem 10 Exercise 2 End