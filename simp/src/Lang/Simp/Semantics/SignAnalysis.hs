module Lang.Simp.Semantics.SignAnalysis where

import Control.Monad hiding (join)
import qualified Data.Map as DM
import qualified Data.Set as DS 
import Lang.Simp.Lattice.CompleteLattice
import Lang.Simp.Lattice.SignLattice
import Lang.Simp.IR.PseudoAssembly
import Lang.Simp.IR.CFG


type AbstractState = DM.Map String SignAbsVal



-- | lookup the sign of a name from an abstract state, if not found, bot is returned
getSign :: AbstractState -> String -> SignAbsVal
getSign m n = case DM.lookup n m of
    Nothing -> Bot
    Just v  -> v

-- | lookup the sign of a operand from an abstract state.
getOprSign :: AbstractState -> Opr -> Either String SignAbsVal
getOprSign m (IntLit v)
    | v > 0                     = Right Plus
    | v == 0                    = Right Zero
    | otherwise                 = Right Minus
getOprSign m (Regstr n)         = Left ("getOprSign failed. Trying to retrieve the sign from an register " ++ n ++ ".")
getOprSign m (Temp (AVar n))    = Right (getSign m n)


-- | Mapping label to abstract state, each entry in this map is an s_i where i is the label.
-- Note this is also a map lattice
type AbstractEnv = DM.Map Label AbstractState

-- | join(s) = \sqbigcup_{t \in pred(s)} t
join :: [AbstractState] -> AbstractState
join = foldl lub DM.empty


type MonotoneFunction = AbstractEnv -> Either String AbstractEnv

-- | Generate the monotone function from a PA program p
genMonotoneFunction :: [LabeledInstr] -> MonotoneFunction
genMonotoneFunction p =
    let cfg  = buildCFG p
        vars = DS.toList $ DS.fromList (concatMap (allVars . snd) p)
        s0   :: AbstractState
        s0   = DM.fromList (map (\v -> (v, Top)) vars)
        joinPredStates :: Label -> AbstractEnv -> AbstractState
        joinPredStates label env =
            let preds = predecessors cfg label
                predsStates :: [AbstractState]
                predsStates = case preds of
                    [] -> [s0] -- label is the first label 
                    _  -> concatMap (\p -> case DM.lookup p env of 
                        Nothing -> []
                        Just v  -> [v]) preds
            in join predsStates
        instrState :: AbstractEnv -> LabeledInstr -> Either String AbstractEnv
        -- ^ case l:t <- src:   s_l = join(s_l)[t -> join(s_l)(src)]
        instrState acc (label, IMove (Temp (AVar t)) src) = do 
            let joinedPredsStates = joinPredStates label acc
            sign <- getOprSign joinedPredsStates src
            return (DM.insert label (DM.insert t sign joinedPredsStates ) acc)
        -- Cohort Problem 10 Exercise 3 TODO 
        -- ^ case l: t <- src1 op src2:  s_l = join(s_l)[t -> join(s_l)(src1) abs(op) join(s_l)(src1)]
        -- ^ other cases: s_l = join(s_l)
        instrState acc (label, _) = 
            let joinedPredsStates = joinPredStates label acc
            in return $ DM.insert label joinedPredsStates acc 
        -- Cohort Problem 10 Exercise 3 END 
    in \absEnv -> foldM instrState absEnv p

-- | abstraction of binary operator +
-- | ++ | $\top$ | + | - | 0 | $\bot$ | 
-- |---|---|---|---|---|---|
-- | $\top$| $\top$| $\top$| $\top$| $\top$| $\bot$| 
-- | + | $\top$ | + | $\top$ | + | $\bot$| 
-- | - | $\top$ | $\top$ | - | - | $\bot$ |
-- | 0 | $\top$ | + | - | 0 | $\bot$ |
-- | $\bot$ | $\bot$ | $\bot$ | $\bot$ | $\bot$ | $\bot$ | 
absPlus :: SignAbsVal -> SignAbsVal -> SignAbsVal
absPlus Top Bot     = Bot 
absPlus Top _       = Top 
absPlus Plus Bot    = Bot
absPlus Plus Zero   = Plus
absPlus Plus Plus   = Plus 
absPlus Plus Top    = Top 
absPlus Plus Minus  = Top
absPlus Minus Bot   = Bot
absPlus Minus Top   = Top 
absPlus Minus Plus  = Top
absPlus Minus Minus = Minus 
absPlus Minus Zero  = Minus
absPlus Zero s2     = s2 
absPlus Bot _       = Bot 


-- | abstraction of binary operator -
-- | -- | $\top$ | + | - | 0 | $\bot$ | 
-- |---|---|---|---|---|---|
-- | $\top$| $\top$| $\top$| $\top$| $\top$| $\bot$| 
-- | + | $\top$ | $\top$ | + | + | $\bot$| 
-- | - | $\top$ | - | $\top$ | - | $\bot$ |
-- | 0 | $\top$ | - | + | 0 | $\bot$ |
-- | $\bot$ | $\bot$ | $\bot$ | $\bot$ | $\bot$ | $\bot$ | 

absMinus :: SignAbsVal -> SignAbsVal -> SignAbsVal 
absMinus Top Bot        = Bot  
absMinus Top _          = Top 
absMinus Plus Bot       = Bot 
absMinus Plus Zero      = Plus 
absMinus Plus Minus     = Plus 
absMinus Plus Plus      = Top 
absMinus Plus Top       = Top 
absMinus Minus Bot      = Bot 
absMinus Minus Zero     = Minus 
absMinus Minus Minus    = Top 
absMinus Minus Plus     = Minus 
absMinus Minus Top      = Top 
absMinus Zero Bot       = Bot 
absMinus Zero Zero      = Zero
absMinus Zero Minus     = Plus
absMinus Zero Plus      = Minus
absMinus Zero Top       = Top 
absMinus Bot _          = Bot 

-- | abstraction of binary operator *
-- | ** | $\top$ | + | - | 0 | $\bot$ | 
-- |---|---|---|---|---|---|
-- | $\top$| $\top$| $\top$| $\top$| 0 | $\bot$| 
-- | + | $\top$ | + | - | 0 | $\bot$| 
-- | - | $\top$ | - | + | 0 | $\bot$ |
-- | 0 | 0 | 0 | 0 | 0 | $\bot$ |
-- | $\bot$ | $\bot$ | $\bot$ | $\bot$ | $\bot$ | $\bot$ | 
absMult :: SignAbsVal -> SignAbsVal -> SignAbsVal
absMult Top Bot         = Bot 
absMult Top Zero        = Zero 
absMult Top _           = Top 
absMult Plus Bot        = Bot 
absMult Plus Zero       = Zero 
absMult Plus Minus      = Minus 
absMult Plus Plus       = Plus 
absMult Plus Top        = Top 
absMult Minus Bot       = Bot 
absMult Minus Zero      = Zero
absMult Minus Minus     = Plus 
absMult Minus Plus      = Minus 
absMult Minus Top       = Top 
absMult Zero Bot        = Bot 
absMult Zero _          = Zero 
absMult Bot _           = Bot 

-- | abstraction of binary operator ==
-- | === | $\top$ | + | - | 0 | $\bot$ | 
-- |---|---|---|---|---|---|
-- | $\top$| $\top$| $\top$| $\top$| $\top$ | $\bot$| 
-- | + | $\top$ | $\top$ | 0 | 0 | $\bot$| 
-- | - | $\top$ | 0 | $\top$ | 0 | $\bot$ |
-- | 0 | $\top$ | 0 | 0 | + | $\bot$ |
-- | $\bot$ | $\bot$ | $\bot$ | $\bot$ | $\bot$ | $\bot$ | 
absDEqual :: SignAbsVal -> SignAbsVal -> SignAbsVal
absDEqual Top Bot       = Bot 
absDEqual Top _         = Top 
absDEqual Plus Bot      = Bot 
absDEqual Plus Zero     = Zero 
absDEqual Plus Minus    = Zero 
absDEqual Plus Plus     = Top 
absDEqual Plus Top      = Top
absDEqual Minus Bot     = Bot
absDEqual Minus Zero    = Zero 
absDEqual Minus Minus   = Top 
absDEqual Minus Plus    = Zero
absDEqual Minus Top     = Top 
absDEqual Zero Bot      = Bot 
absDEqual Zero Zero     = Plus
absDEqual Zero Minus    = Zero 
absDEqual Zero Plus     = Zero 
absDEqual Zero Top      = Top
absDEqual Bot _         = Bot 

-- | abstraction of binary operator < 
-- | << | $\top$ | + | - | 0 | $\bot$ | 
-- |---|---|---|---|---|---|
-- | $\top$| $\top$| $\top$| $\top$| $\top$ | $\bot$| 
-- | + | $\top$ | $\top$ | 0 | 0 | $\bot$| 
-- | - | $\top$ | + | $\top$ | + | $\bot$ |
-- | 0 | $\top$ | + | 0 | 0 | $\bot$ |
-- | $\bot$ | $\bot$ | $\bot$ | $\bot$ | $\bot$ | $\bot$ | 
absLThan :: SignAbsVal -> SignAbsVal -> SignAbsVal
absLThan Top Bot        = Bot 
absLThan Top _          = Top 
absLThan Plus Bot       = Bot 
absLThan Plus Zero      = Zero 
absLThan Plus Minus     = Zero
absLThan Plus Plus      = Top 
absLThan Plus Top       = Top
absLThan Minus Bot      = Bot 
absLThan Minus Zero     = Plus 
absLThan Minus Minus    = Top 
absLThan Minus Plus     = Plus
absLThan Minus Top      = Top
absLThan Zero Bot       = Bot 
absLThan Zero Zero      = Zero 
absLThan Zero Minus     = Zero
absLThan Zero Plus      = Plus
absLThan Zero Top       = Top 
absLThan Bot _          = Bot

-- | Top level function for sign analysis 
--  Peform sign analysis over a PA program `p` return an abstract environment mapping label to abstract states
--   Each abstract state is mapping variable names to sign abstract value.
analyze :: [LabeledInstr] -> Either String AbstractEnv 
analyze p = 
    let f                 = genMonotoneFunction p 
        vars              = DS.toList $ DS.fromList (concatMap (allVars . snd) p)
        labels            = map fst p
        initAbstractState = DM.fromList (map (\v -> (v,Top)) vars)
        initAbstractEnv   = DM.fromList (map (\l -> (l,initAbstractState)) labels)
    in naiveFP f initAbstractEnv