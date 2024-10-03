module Lang.Simp.IR.CFG where 

-- This module contains the implementation of control flow graph utils
--  1. query for successors and descendants
--  2. construct a CFG from a PA program


import qualified Data.Map as DM 
import qualified Data.Set as DS 
import Lang.Simp.IR.PseudoAssembly


-- | a graph is a mapping from a source label to a set of destination labels
type CFG = DM.Map Label [Label]

-- | return the list of predcessor of a vertex v in a graph
predecessors :: CFG -> Label -> [Label] 
predecessors g v = concatMap (\ (k, vs) -> 
    if v `elem` vs 
    then [k]
    else []) (DM.toList g)


-- | return the list of successors of a vertex v in a graph 
successors :: CFG -> Label -> [Label] 
successors g v = case DM.lookup v g of 
    Nothing -> [] 
    Just vs -> vs 


-- | find all descendants of vertex v in graph g
descendants :: CFG -> Label -> DS.Set Label 
descendants g v = go DS.empty [v]
    where 
        go acc vs = 
            let newVs = filter (\x -> not (x `DS.member` acc)) vs 
            in case newVs of 
                [] -> acc 
                (_:_) -> 
                    let next = concatMap (\x -> successors g x) newVs 
                    in go (acc `DS.union` DS.fromList vs) next


-- | constructing a CFG from a PA program
buildCFG :: [LabeledInstr] -> CFG 
buildCFG p = foldl go DM.empty p
    where 
        go :: CFG -> LabeledInstr -> CFG
        go acc (label, IRet)                = acc
        go acc (label, IGoto label2)        = case DM.lookup label acc of 
            Nothing -> DM.insert label [label2] acc
            Just labels -> DM.insert label (label2:labels) acc
        go acc (label, IIfNot cond label2)  = 
            let label1 = label + 1
            in case DM.lookup label acc of 
                Nothing     -> DM.insert label [label1, label2] acc 
                Just labels -> DM.insert label (label1:label2:labels) acc 
        go acc (label, i)                   = 
            let label1 = label + 1
            in case DM.lookup label acc of 
                Nothing     -> DM.insert label [label1] acc
                Just labels -> DM.insert label (label1:labels) acc 

