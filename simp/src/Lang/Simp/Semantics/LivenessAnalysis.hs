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
join = undefined -- fixme 
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
        -- ^ case l:t <- src:   s_l = join(s_l) - {t} \cup vars(src)
        instrState acc (label, IMove (Temp (AVar t)) src) = do
            let joinedSuccStates = joinSuccStates label acc
            return (DM.insert label ((DS.delete t joinedSuccStates) `DS.union` (DS.fromList (vars src))) acc)
        -- ^ case l: t <- src1 op src2:  s_l = join(s_l) - {t} \cup vars(src1) \cup vars(src2)
        -- ^ case l: r <- src1 op src2:  s_l = join(s_l) \cup vars(src1) \cup vars(src2)
        -- ^ case l: ifn t goto l':  s_l = join(s_l) \cup {t}
        -- ^ other cases: s_l = join(s_l)
        instrState acc (label, instr) = undefined -- fixme 
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