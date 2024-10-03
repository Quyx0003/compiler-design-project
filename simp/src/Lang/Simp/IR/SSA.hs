module Lang.Simp.IR.SSA where

import qualified Data.Map as DM
import qualified Data.Set as DS
import Control.Monad
import Data.List
import Lang.Simp.IR.PseudoAssembly
import Lang.Simp.IR.CFG
import Lang.Simp.IR.DF

-- this module defines the unstructured SSA and its construction following Cytron's algorithm

-- | PhiAssignment data type, consists of the lhs, the operand list and the stem of the lhs (before renaming)
data PhiAssignment = PhiAssignment Opr [(Label, AVar)] Opr
    deriving (Show, Eq)

type SSALabeledInstr = (Label, [PhiAssignment], Instr)


-- | SSA construction using Cytron's algorithm
-- In the notes the SSA construction is divided into two steps, 1. inserting phi assignment 2. renaming  variable.

buildSSA :: [LabeledInstr] -> Either String [SSALabeledInstr]
buildSSA pa =
    let cfg = buildCFG pa
    in do
        dt <- buildDomTree cfg 1
        let dft = buildDFT dt cfg
            -- for each labeld instruction convert it to SSA labeled instruction by adding empty phi assignments 
            p   = insertPhis pa dft cfg
        q  <- convDTree DM.empty dt p cfg
        return (map snd (sortOn fst (DM.toList q)))

-- Part 1 : insert Phi assignments

-- | the E environment: mapping a label l to a list of variables that should have phi-assignments in l 
type E = DM.Map Label [String]

-- | the P environment: mapping a label l to a SSA labeld instruction
type P = DM.Map Label SSALabeledInstr


-- | generatae a raw SSA program from a Peudo Assembly program pa, variables are yet to be renamed. 
insertPhis :: [LabeledInstr] -> DFTable -> CFG -> P
insertPhis pa dft g =
        -- a list of pairs. Each pair consists of a label l and the set of variables that are modified in l
    let labelsModdedVars :: [(Label, [String])]
        labelsModdedVars = map modVars pa
        -- Lab 3 Task 1.2 TODO 
        e :: E
        e = undefined -- fixme
        -- Lab 3 Task 1.2 TODO 
        paWithPhis :: [SSALabeledInstr]
        paWithPhis = map (\ (l,i) -> case DM.lookup l e of
            { Nothing   -> (l, [], i)
            ; Just vars ->
                let phis = undefined -- fixme
                in (l, phis, i)
            }) pa
        -- Lab 3 Task 1.2 END
    in foldl (\acc (l, phis, i) -> DM.insert l (l, phis, i) acc) DM.empty paWithPhis

modVars :: LabeledInstr -> (Label, [String])
modVars (label, IMove (Temp (AVar n)) _)        = (label, [n])
modVars (label, IPlus (Temp (AVar n)) _ _ )     = (label, [n])
modVars (label, IMinus (Temp (AVar n)) _ _ )    = (label, [n])
modVars (label, IMult (Temp (AVar n)) _ _ )     = (label, [n])
modVars (label, IDEqual (Temp (AVar n)) _ _ )   = (label, [n])
modVars (label, ILThan (Temp (AVar n)) _ _ )    = (label, [n])
modVars (label, _ )                             = (label, [])


-- | Part 2 variable renaming 


-- As we are using FP for the implementation, the 2nd step of cytron's algorithm presented in the notes an be 
-- simplified w/o using explicit (user defined) stacks.
-- The differeces are summarized as
--
--      notes                                                   FP implementation 
-- -----|------------------------------------------------------|-------------------------------------------
-- K   | mapping from var to stack of numbers                 | mapping from var to latest number 
-- vars| local set of varables to be popped at the end of     | local set of variables (no need to pop)


{-
-- K - as above
-- P - the PA SSA program (mapping from label to label instructions)
-- G - the CFG
-- 
-- K, P(l) |- K', li          P^0 = P oplus (l, li)
-- for i in [0,n], (l, succ_i) in G, we have  K', P^i(succ_i) |-phi_rhs li_i, P^{i+1} = P^i oplus (succ_i, li_i)
-- for j in [0,k], ch_j in chs, we have  K', ch_j, P^{n+j}, G |- P^{n+j+1}
-- -------------------------------------------------------------------------------------------- (DTree)
-- K, Node(l,chs), P, G |- P^{n+k} 
-}

type K = DM.Map String Int
type Vars = DS.Set AVar

convDTree :: K -> DomTree -> P -> CFG -> Either String P
convDTree _ Empty p _ = Right p
convDTree k (Node l children) p g = case DM.lookup l p of
    { Nothing -> Left ("buildSSA failed. Label " ++ show l ++ " is not found.")
    ; Just li -> do
        (k_p, li_p) <- convInstr k li
        let p0    = DM.insert l li_p p
            succs = successors g l
        pnj         <- foldM (\ acc succ -> case DM.lookup succ acc of
            { Nothing -> Left ("convDTree failed: node " ++ show l ++ "'s child "++ show succ ++ " does not exists.")
            ; Just li -> do
                li_p <- convPhiRhs k_p li l
                return (DM.insert succ li_p acc)
            }) p0 succs
        foldM (\acc child -> convDTree k_p child acc g) pnj children
    }


convInstr :: K -> SSALabeledInstr -> Either String (K, SSALabeledInstr)
convInstr k (label, phis, IMove (IntLit v) s)       = Left "convInstr failed. Int literal appears as the LHS of the IMove."
convInstr k (label, phis, IPlus (IntLit v) s1 s2)   = Left "convInstr failed. Int literal appears as the LHS of the IPlus."
convInstr k (label, phis, IMinus (IntLit v) s1 s2)  = Left "convInstr failed. Int literal appears as the LHS of the IMinus."
convInstr k (label, phis, IMult (IntLit v) s1 s2)   = Left "convInstr failed. Int literal appears as the LHS of the IMult."
convInstr k (label, phis, IDEqual (IntLit v) s1 s2) = Left "convInstr failed. Int literal appears as the LHS of the IDEqual."
convInstr k (label, phis, ILThan (IntLit v) s1 s2)  = Left "convInstr failed. Int literal appears as the LHS of the ILThan."
{-
K, phis |-phi_lhs K', phis'   s' = ren(K', s)
---------------------------------------- (reg1)
K, l:phis r <-s |- K', l:phis' r <- s'
-}
convInstr k (label, phis, IMove (Regstr r) s)       = do
    (k', phis') <- convPhiLhs k phis
    s'         <- ren k' s
    return (k', (label, phis', IMove (Regstr r) s'))
{-
K, phis |-phi_lhs K', phis'   s1' = ren(K',s1) s2' = ren(K', s2)
-------------------------------------------------------------- (reg2)
K, l:phis r <-s1 op s2 |- K', l:phis' r <- s1' op s2'
-}
convInstr k (label, phis, IPlus (Regstr r) s1 s2)   = do
    (k', phis') <- convPhiLhs k phis
    s1'         <- ren k' s1
    s2'         <- ren k' s2
    return (k', (label, phis', IPlus (Regstr r) s1' s2'))
convInstr k (label, phis, IMinus (Regstr r) s1 s2)  = do
    (k', phis') <- convPhiLhs k phis
    s1'         <- ren k' s1
    s2'         <- ren k' s2
    return (k', (label, phis', IMinus (Regstr r) s1' s2'))
convInstr k (label, phis, IMult (Regstr r) s1 s2)   = do
    (k', phis') <- convPhiLhs k phis
    s1'         <- ren k' s1
    s2'         <- ren k' s2
    return (k', (label, phis', IMult (Regstr r) s1' s2'))
convInstr k (label, phis, IDEqual (Regstr r) s1 s2) = do
    (k', phis') <- convPhiLhs k phis
    s1'         <- ren k' s1
    s2'         <- ren k' s2
    return (k', (label, phis', IDEqual (Regstr r) s1' s2'))
convInstr k (label, phis, ILThan (Regstr r) s1 s2)  = do
    (k', phis') <- convPhiLhs k phis
    s1'         <- ren k' s1
    s2'         <- ren k' s2
    return (k', (label, phis', ILThan (Regstr r) s1' s2'))
{-
K, phis |-phi_lhs K', phis'   s' = ren(K', s)
K'' = K' oplus (t, K(t)+1)    t_i = ren(K'', t)
---------------------------------------- (TVar1)
K, l:phis t <-s |- K'', l:phis' t_i <- s'
-}
convInstr k (label, phis, IMove (Temp (AVar n)) s)  = do
    (k', phis') <- convPhiLhs k phis
    s'          <- ren k' s
    k''         <- incr k' n
    t_i         <- ren k'' (Temp (AVar n))
    return (k'', (label, phis', IMove t_i s'))
{-
K, phis |-phi_lhs K', phis'   s1' = ren(K', s1)   s2' = ren(K', s2)
K'' = K' oplus (t, K(t)+1)    t_i = ren(K'', t)
---------------------------------------- (TVar2)
K, l:phis t <-s1 op s2 |- K'', l:phis' t_i <- s1' op s2'
-}
convInstr k (label, phis, IPlus (Temp (AVar n)) s1 s2)   = do
    (k', phis') <- convPhiLhs k phis
    s1'         <- ren k' s1
    s2'         <- ren k' s2
    k''         <- incr k' n
    t_i         <- ren k'' (Temp (AVar n))
    return (k'', (label, phis', IPlus t_i s1' s2'))
convInstr k (label, phis, IMinus (Temp (AVar n)) s1 s2)  = do
    (k', phis') <- convPhiLhs k phis
    s1'         <- ren k' s1
    s2'         <- ren k' s2
    k''         <- incr k' n
    t_i         <- ren k'' (Temp (AVar n))
    return (k'', (label, phis', IMinus t_i s1' s2'))
convInstr k (label, phis, IMult (Temp (AVar n)) s1 s2)   = do
    (k', phis') <- convPhiLhs k phis
    s1'         <- ren k' s1
    s2'         <- ren k' s2
    k''         <- incr k' n
    t_i         <- ren k'' (Temp (AVar n))
    return (k'', (label, phis', IMult t_i s1' s2'))
convInstr k (label, phis, IDEqual (Temp (AVar n)) s1 s2) = do
    (k', phis') <- convPhiLhs k phis
    s1'         <- ren k' s1
    s2'         <- ren k' s2
    k''         <- incr k' n
    t_i         <- ren k'' (Temp (AVar n))
    return (k'', (label, phis', IDEqual t_i s1' s2'))
convInstr k (label, phis, ILThan (Temp (AVar n)) s1 s2)  = do
    (k', phis') <- convPhiLhs k phis
    s1'         <- ren k' s1
    s2'         <- ren k' s2
    k''         <- incr k' n
    t_i         <- ren k'' (Temp (AVar n))
    return (k'', (label, phis', ILThan t_i s1' s2'))
{-
 K, phis |-phi_lhs K', phis' 
----------------------------------------- (Goto)
 K, l: phis goto l' |- K', l:phis' goto l'
-}
convInstr k (label, phis, IGoto m)                       = do
    (k', phis') <- convPhiLhs k phis
    return (k', (label, phis', IGoto m))
{-
K, phis |-phi_lhs K', phis'    t' = ren(K', t)
----------------------------------------------- (IfNGoto)
K, l: phis ifn t goto l' |- K', l:phis' goto l'
-}
convInstr k (label, phis, IIfNot t m)                    = do
    (k', phis') <- convPhiLhs k phis
    t'          <- ren k' t
    return (k', (label, phis', IIfNot t' m))
{-
K, phis |-phi_lhs K', phis' 
----------------------------------------- (Ret)
K, l: phis ret |- K', l:phis' ret
-}
convInstr k (label, phis, IRet)                          = do
    (k', phis') <- convPhiLhs k phis
    return (k', (label, phis', IRet))



-- | increment the counter under name in k by 1
-- | if the name does not exist, set it to 1
incr :: K -> String -> Either String K
incr k name = case DM.lookup name k of
    Nothing -> Right (DM.insert name 1 k)
    Just i  -> Right (DM.insert name (i+1) k)


{-
convert the LHS of the phi assignments
 
---------------------------------(PLhsPhi1)
K, [] |-  K, []
 
K' = K oplus (x -> K(x) + 1)
K', phis |- K'', phis'
---------------------------------------------------------------------------(PLhsPhi2)
K, x = phi(l1:x1, l2:x2); phis |-  K'', x_i = phi(l1:x1, l2:x2); phis'

Note that we do not rename the RHS operands of the phi assignments, which is handled in the PRhs rules.
-}
convPhiLhs :: K -> [PhiAssignment] -> Either String (K, [PhiAssignment])
convPhiLhs k phis = do
    (k2, phis2) <- foldM go acc0 phis
    return (k2, sortOn (\ phi -> case phi of
        { PhiAssignment dest operands stem -> stem }) phis2)
    where
        acc0 = (k, [])
        go :: (K, [PhiAssignment]) -> PhiAssignment -> Either String (K, [PhiAssignment])
        go (k1, phis1) (PhiAssignment (IntLit v) operands stem)         = Left "convPhis failed. Integer literal appears in the LHS of the phi assignment."
        go (k1, phis1) (PhiAssignment (Regstr name) operands stem)      = Left ("convPhis failed. Register " ++ name ++ " appears in the LHS of the phi assignment.")
        go (k1, phis1) (PhiAssignment (Temp (AVar name)) operands stem) = do
            k2  <- incr k1 name
            x_i <- ren k2 (Temp (AVar name))
            return (k2, phis1 ++ [PhiAssignment x_i operands stem])


{-
 convert the RHS of the phi assignments in labeled instruction li
 
 K, phis, l |-phi_rhs phis'
 ------------------------------------------(PRhs)
 K, l: phis i, l' |-phi_rhs l: phis' i
-}

convPhiRhs :: K -> SSALabeledInstr -> Label -> Either String SSALabeledInstr
convPhiRhs k (label, phis, instr) l' = do
    phis' <- foldM go [] phis
    return (label, phis', instr)
    where
        go :: [PhiAssignment] -> PhiAssignment -> Either String [PhiAssignment]
        {-
         --------------------------------- (PPhiRhs1)
         K, [] |- K, []
         
         K, phis, l1 |- phis'   x1' = ren(K,x1)
         ---------------------------------------------------------------------- (PPhiRhs2)
         K, x = phi(l1:x1, l2:x2); phis, l1 |- x = phi(l1:x1', l2:x2); phis'
        
         K, phis, l2 |- phis'   x2' = ren(K,x2)
         ---------------------------------------------------------------------- (PPhiRhs3)
         K, x = phi(l1:x1, l2:x2); phis, l2 |- x = phi(l1:x1, l2:x2'); phis'

         stem(x) \not in dom(K)
         ---------------------------------------------------------------------- (PPhiRhs4)
         K, x = phi(l1:x1, l2:x2); phis, l3 |- x = phi(l1:x1, l2:x2'); phis'
        -}
        go acc_phis (PhiAssignment _ operands (Temp (AVar n)))
            | not (n `DM.member` k) = Right acc_phis -- (PPhiRhs4) case 
        go acc_phis (PhiAssignment x operands stem) = do
            operands' <- foldM (\ acc_operands p -> case p of
                { (m, avar) | m == l' -> do
                    avar' <- renAVar k avar
                    return (acc_operands ++ [(m, avar')])
                            | otherwise -> Right (acc_operands ++ [(m,avar)])
                }) [] operands
            let operands_sorted' = sortOn fst operands'
            return (acc_phis ++ [PhiAssignment x operands_sorted' stem])



-- | rename an operand 

ren :: K -> Opr -> Either String Opr
ren k (IntLit v)    = Right (IntLit v)
ren k (Regstr name) = Right (Regstr name)
ren k (Temp avar)   = do
    avar' <- renAVar k avar
    return (Temp avar')


-- | rename an avar
renAVar :: K -> AVar -> Either String AVar
renAVar k (AVar "input") = Right (AVar "input") -- we don't rename input 
renAVar k (AVar n)       = case DM.lookup n k of
    Nothing -> Left ("renaming failed. temp variable " ++ n ++ " is not found in the K environment " ++ show k ++ ".")
    Just i  -> Right (AVar (n ++ "_" ++ show i))


-- | SSA destruction 
destructSSA :: [SSALabeledInstr] -> Either String [LabeledInstr]
destructSSA p = label (phiMigrate p)


-- | Phi migration 
phiMigrate :: [SSALabeledInstr] -> [LabeledInstr]
phiMigrate p = -- for each l: phis i in P, append l: i to Q 
    let q :: [LabeledInstr]
        q = map (\ (l, phis, i) -> (l,i)) p
        -- for each l: phis i in P
        -- for each x = phi(l1:x1, l2:x2) in phis, append l1: x = x1 and l2: x = x2 to q
        q1 :: [LabeledInstr]
        q1 = foldl (\ acc (l, phis, i) ->
                    foldl (\ acc1 (PhiAssignment d oprs stem) ->
                            acc1 ++ map (\ (label, avar) -> (label, IMove d (Temp avar))) oprs)
                            acc phis) q p
    in sortOn fst q1


-- | relabeling
-- | precondition, p is sorted according to label 
-- |               labels in p might not be unique
label ::[LabeledInstr] -> Either String [LabeledInstr]
label p =
    -- new labels 
    let newLabels = [1 .. (length p)]
        -- old labels 
        oldLabels = map fst p
        m :: DM.Map Label Label
        m = DM.fromList (zip newLabels oldLabels)
        -- go 
        go :: LabeledInstr -> Either String LabeledInstr
        go (label, i) = case DM.lookup label m of
            Nothing         -> Left ("relabeling error: " ++ show label ++ " not found.")
            Just newLabel   -> case labelInstr i m of
                Left err    -> Left err
                Right j     -> Right (newLabel, j)

    in foldM ( \acc li -> do
        li1 <- go li
        return (acc ++ [li1])) [] p

-- | labelling instructions 
labelInstr :: Instr -> DM.Map Label Label -> Either String Instr
labelInstr (IGoto dest) m       = case DM.lookup dest m of
    Nothing     -> Left ("relabeling error: " ++ show dest ++ " not found.")
    Just dest1  -> Right (IGoto dest1)
labelInstr (IIfNot cond dest) m = case DM.lookup dest m of
    Nothing     -> Left ("relabeling error: " ++ show dest ++ " not found.")
    Just dest1  -> Right (IIfNot cond dest1)
labelInstr i m                  = Right i