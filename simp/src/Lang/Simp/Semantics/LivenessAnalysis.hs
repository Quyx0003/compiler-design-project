module Lang.Simp.Semantics.LivenessAnalysis where

import Control.Monad hiding (join)
import qualified Data.Map as DM
import qualified Data.Set as DS
import Lang.Simp.Lattice.CompleteLattice
import Lang.Simp.IR.PseudoAssembly
import Lang.Simp.IR.CFG




-- | abstract state is a set of variable names 
type AbstractState = DS.Set String

-- | abstract environment is a mapping from label to abstract state
type AbstractEnv = DM.Map Label AbstractState


-- Lab 3 Task 2.1 
-- | join(s) = \sqbigcup_{t \in succ(s)} t
join :: [AbstractState] -> AbstractState
join = DS.unions
-- Lab 3 Task 2.1 end


type MonotoneFunction = AbstractEnv -> Either String AbstractEnv

-- | generate the monotone function from a PA program p 
genMonotoneFunction :: [LabeledInstr] -> MonotoneFunction
genMonotoneFunction p =
    let cfg = buildCFG p
        top = DS.toList $ DS.fromList (concatMap (allVars . snd) p)
        joinSuccStates :: Label -> AbstractEnv -> AbstractState
        joinSuccStates label env =
            let succs = successors cfg label
                succsStates = concatMap (\succ -> case DM.lookup succ env of
                    Nothing -> [DS.empty]
                    Just value -> [value]) succs
            in join succsStates
        -- Lab 3 Task 2.2 
        instrState :: AbstractEnv -> LabeledInstr -> Either String AbstractEnv
        -- Case l: t <- s: s_l = join(s_l) - {t} ∪ vars(s)
        instrState acc (label, IMove (Temp (AVar t)) src) = do
            let joinedSuccStates = joinSuccStates label acc
            return $ DM.insert label (DS.delete t joinedSuccStates `DS.union` DS.fromList (vars src)) acc

        -- Handle cases where the destination is not a Temp (AVar t)
        instrState acc (label, IMove _ src) = do
            let joinedSuccStates = joinSuccStates label acc
            return $ DM.insert label (joinedSuccStates `DS.union` DS.fromList (vars src)) acc

        -- Case l: t <- s1 op s2: s_l = join(s_l) - {t} ∪ vars(s1) ∪ vars(s2)
        instrState acc (label, IPlus (Temp (AVar t)) src1 src2) = do
            let joinedSuccStates = joinSuccStates label acc
            return $ DM.insert label (DS.delete t joinedSuccStates `DS.union` DS.fromList (vars src1 ++ vars src2)) acc
        instrState acc (label, IMinus (Temp (AVar t)) src1 src2) = do
            let joinedSuccStates = joinSuccStates label acc
            return $ DM.insert label (DS.delete t joinedSuccStates `DS.union` DS.fromList (vars src1 ++ vars src2)) acc
        instrState acc (label, IMult (Temp (AVar t)) src1 src2) = do
            let joinedSuccStates = joinSuccStates label acc
            return $ DM.insert label (DS.delete t joinedSuccStates `DS.union` DS.fromList (vars src1 ++ vars src2)) acc
        instrState acc (label, IDEqual (Temp (AVar t)) src1 src2) = do
            let joinedSuccStates = joinSuccStates label acc
            return $ DM.insert label (DS.delete t joinedSuccStates `DS.union` DS.fromList (vars src1 ++ vars src2)) acc
        instrState acc (label, ILThan (Temp (AVar t)) src1 src2) = do
            let joinedSuccStates = joinSuccStates label acc
            return $ DM.insert label (DS.delete t joinedSuccStates `DS.union` DS.fromList (vars src1 ++ vars src2)) acc

        -- Case l: ifn t goto l': s_l = join(s_l) ∪ vars(t)
        instrState acc (label, IIfNot (Temp (AVar t)) _) = do
            let joinedSuccStates = joinSuccStates label acc
            return $ DM.insert label (joinedSuccStates `DS.union` DS.singleton t) acc

        -- Case l: ret: s_l = join(s_l)
        instrState acc (label, IRet) = do
            let joinedSuccStates = joinSuccStates label acc
            return $ DM.insert label joinedSuccStates acc

        -- Case l: goto l': s_l = join(s_l)
        instrState acc (label, IGoto _) = do
            let joinedSuccStates = joinSuccStates label acc
            return $ DM.insert label joinedSuccStates acc

        -- Other instructions default to join(s_l)
        instrState acc (label, _) = do
            let joinedSuccStates = joinSuccStates label acc
            return $ DM.insert label joinedSuccStates acc
        -- Lab 3 Task 2.2 end

    in \absEnv -> foldM instrState absEnv p


-- | Top level function for liveness analysis 
--  Peform livness analysis over a PA program `p` return an abstract environment mapping label to abstract states
--   Each abstract state is mapping a set of live variables
analyze :: [LabeledInstr] -> Either String AbstractEnv
analyze p =
    let f                 = genMonotoneFunction p
        vars              = DS.toList $ DS.fromList (concatMap (allVars . snd) p)
        labels            = map fst p
        initAbstractState = DS.empty
        initAbstractEnv   = DM.fromList (map (\l -> (l,initAbstractState)) labels)
    in naiveFP f initAbstractEnv