module Lang.Simp.Lattice.CompleteLattice where

import qualified Data.Set as DS 
import qualified Data.Map as DM 

-- | complete lattice 
-- | (s, sqsubseteq)
-- | sqsubseteq : s -> s -> Bool compute the partial order
-- |   x `sqsubseteq` y == True iff x is below y in the lattice
-- |   x `sqsubseteq` y == False iff x is not below y in the lattice
-- | lub : s -> s -> s  returns the least upper bound, also known as the join
-- | glb : s -> s -> s  returns the greatest lower bound (not implemented), also known as the meat
class CompleteLattice s where
    sqSubsetEq  :: s -> s -> Bool 
    lub         :: s -> s -> s 
    eq          :: s -> s -> Bool 
    eq a b      = sqSubsetEq a b && sqSubsetEq b a 

-- | naive FP algorithm
-- | it is different from the note: the monotone function f's type is s -> Either String s instead of s -> s, 
-- | i.e. it might fail 
naiveFP :: (CompleteLattice s, Show s) => (s -> Either String s) 
        -> s 
        -> Either String s
naiveFP f a = do 
    b <- f a
    if eq a b
    then Right a
    else naiveFP f b


instance Ord a => CompleteLattice (DS.Set a) where 
    sqSubsetEq = DS.isSubsetOf
    lub = DS.union
         

instance (Ord k, CompleteLattice a) => CompleteLattice (DM.Map k a) where
    sqSubsetEq ma mb = 
        let keys = DS.toList (DS.fromList (DM.keys ma ++ DM.keys mb)) 
            go k = case (DM.lookup k ma, DM.lookup k mb) of 
                (Nothing, _)     -> True -- absence of key implies bottom 
                (_, Nothing)     -> False 
                (Just a, Just b) -> a `sqSubsetEq` b
        in all go keys
    lub ma mb = 
        let keys = DS.toList (DS.fromList (DM.keys ma ++ DM.keys mb)) 
            go acc k = case (DM.lookup k ma, DM.lookup k mb) of 
                (Nothing, Nothing)  -> acc 
                (Just a,  Nothing)  -> DM.insert k a acc 
                (Nothing, Just b)   -> DM.insert k b acc 
                (Just a, Just b)    -> DM.insert k (a `lub` b) acc 
        in foldl go DM.empty keys
