{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
module Lang.Simp.Semantics.TypeInf where 

import qualified Data.Map as DM
import qualified Data.Set as DS 
import Control.Monad

import Lang.Simp.Syntax.AST  

-- | simple type
data Type = IntTy | BoolTy 
    deriving (Show, Eq, Ord)

-- | extended type 
data ExType = MonoType Type | TypeVar String 
    deriving (Show, Eq, Ord)

type TypeEnv = DM.Map Var Type 


-- | type constraints
type TypeConstrs = DS.Set (ExType, ExType)



-- | extract type variables type from constaints
getTVars :: TypeConstrs -> DS.Set String 
getTVars tcs = 
    DS.unions (map go (DS.toList tcs)) 
    where 
        go :: (ExType, ExType) -> DS.Set String
        go (TypeVar n1, TypeVar n2) = DS.fromList [n1,n2] 
        go (TypeVar n1, _)          = DS.singleton n1
        go (_, TypeVar n2)          = DS.singleton n2  
        go (_, _)                   = DS.empty


-- | type substitution 
data TypeSubst = Empty -- ^ [] 
    | RevComp (String, ExType) TypeSubst -- ^ psi compose [type/a]
    deriving (Show, Eq)


-- | make a singleton type substitution 
singleton :: String -> ExType -> TypeSubst 
singleton name ty = RevComp (name,ty) Empty 


-- | composition of two type substitutions
-- | 
compose :: TypeSubst -> TypeSubst -> TypeSubst 
compose ts1 Empty = ts1 
compose ts1 (RevComp s ts2) = RevComp s (compose ts1 ts2) 



class Substitutable a where 
    applySubst :: TypeSubst -> a -> a 

-- | apply type subst to an extended type
-- Lab 2 Task 2.1
instance Substitutable ExType where
    applySubst :: TypeSubst -> ExType -> ExType
    applySubst Empty t = t
    applySubst (RevComp (alp1, t1) Empty) (MonoType t2) = MonoType t2
    applySubst (RevComp (alp1, t) Empty) (TypeVar alp2) 
        | alp1 == alp2 = t
        | otherwise    = TypeVar alp2
    applySubst (RevComp (alp1, t) psi) ty =
        let ty'  = applySubst (RevComp (alp1, t) Empty) ty in
        applySubst psi ty' 
        
-- Lab 2 Task 2.1 end

instance (Substitutable a, Substitutable b) => Substitutable (a,b) where
    applySubst psi (a,b) = (applySubst psi a, applySubst psi b) 


instance (Substitutable a) => Substitutable [a] where 
    applySubst psi as = map (applySubst psi) as 


-- | type inference type class 
class Infer a where 
    infer :: a -> TypeConstrs 


instance (Infer a) => Infer [a] where 
    infer as = foldl (\acc a -> DS.union acc (infer a)) DS.empty as 


instance Infer Stmt where 
    infer Nop               = DS.empty 
    infer (Assign x e)      = 
        let n = varname x
            alphax = TypeVar n 
        in case inferExp e of 
            (exTy, k) -> DS.insert (alphax, exTy) k
    infer (Ret x)           = DS.empty 
    -- Lab 2 Task 2.3
    -- While condition
    infer (While condition body) = 
        let 
            (condType, condConstrs) = inferExp condition
            condBoolConstraint = DS.singleton (condType, MonoType BoolTy) -- Create an entry of set
            bodyConstrs = infer body
        in 
            DS.union condBoolConstraint (DS.union condConstrs bodyConstrs)
    -- If condition
    infer (If condition thenBranch elseBranch) = 
        let 
            (condType, condConstrs) = inferExp condition
            condBoolConstraint = DS.singleton (condType, MonoType BoolTy)
            thenBranchConstrs = infer thenBranch
            elseBranchConstrs = infer elseBranch
        in 
            DS.union condBoolConstraint (DS.union condConstrs (DS.union thenBranchConstrs elseBranchConstrs))
    -- Lab 2 Task 2.3 end 


inferExp :: Exp -> (ExType, TypeConstrs)
inferExp (ConstExp (IntConst v))    = (MonoType IntTy, DS.empty)
inferExp (ConstExp (BoolConst v))   = (MonoType BoolTy, DS.empty)
inferExp (VarExp v) = 
    let n = varname v
    in (TypeVar n, DS.empty)
inferExp (ParenExp e)               = inferExp e
-- Lab 2 Task 2.3
inferExp (Minus e1 e2)              = 
    case (inferExp e1, inferExp e2) of 
        ((exty1, k1), (exty2, k2)) -> 
            let k = DS.fromList [(exty1, MonoType IntTy), (exty2, MonoType IntTy)] `DS.union` k1 `DS.union` k2
            in (MonoType IntTy, k)
inferExp (Plus e1 e2)               = 
    case (inferExp e1, inferExp e2) of 
        ((exty1, k1), (exty2, k2)) -> 
            let k = DS.fromList [(exty1, MonoType IntTy), (exty2, MonoType IntTy)] `DS.union` k1 `DS.union` k2
            in (MonoType IntTy, k)
inferExp (Mult e1 e2)               = 
    case (inferExp e1, inferExp e2) of 
        ((exty1, k1), (exty2, k2)) -> 
            let k = DS.fromList [(exty1, MonoType IntTy), (exty2, MonoType IntTy)] `DS.union` k1 `DS.union` k2
            in (MonoType IntTy, k)
inferExp (DEqual e1 e2)              = 
    case (inferExp e1, inferExp e2) of 
        ((exty1, k1), (exty2, k2)) -> 
            let k = DS.fromList [(exty1, exty2)] `DS.union` k1 `DS.union` k2
            in (MonoType BoolTy, k)
inferExp (LThan e1 e2)              = 
    case (inferExp e1, inferExp e2) of 
        ((exty1, k1), (exty2, k2)) -> 
            let k = DS.fromList [(exty1, exty2)] `DS.union` k1 `DS.union` k2
            in (MonoType BoolTy, k)
-- Lab 2 Task 2.3 end 


-- | Unification type class 
class Unifiable a where
    mgu :: a -> Either String TypeSubst


-- | unify two extypes 
-- Lab 2 Task 2.2 
instance Unifiable (ExType,ExType) where
    mgu :: (ExType, ExType) -> Either String TypeSubst
    mgu (MonoType IntTy, MonoType IntTy)   = Right Empty
    mgu (MonoType BoolTy, MonoType BoolTy) = Right Empty
    mgu (TypeVar s, exTy)                  = Right (singleton s exTy)
    mgu (exTy, TypeVar s)                  = Right (singleton s exTy)
    mgu (exTy1, exTy2) = Left ("error: unable to unify " ++ show exTy1 ++ " with " ++ show exTy2)

-- | unifying a list of unifaibles
instance (Unifiable a, Substitutable a) => Unifiable [a] where
    mgu :: (Unifiable a, Substitutable a) => [a] -> Either String TypeSubst
    mgu []      = Right Empty 
    mgu (x:xs) = do
        psi1 <- mgu x
        let xs' = map (applySubst psi1) xs
        psi2 <- mgu xs'
        -- Compose Ψ2 with Ψ1
        return (compose psi2 psi1)
-- Lab 2 Task 2.2 end


-- | unifying a set of type constraints (i.e. a set of (ExType, ExType))
instance Unifiable TypeConstrs where 
    mgu tyconstrs = mgu (DS.toList tyconstrs)



-- | grounding a type variable's name given a type substitution 
ground :: String -> TypeSubst -> Either String Type
ground name subst = case applySubst subst (TypeVar name) of 
    MonoType t -> Right t 
    _          -> Left $ "error: type inference failed. " ++ name ++ "'s type cannot be grounded with " ++ show subst


typeInf :: [Stmt] -> Either String TypeEnv 
typeInf s = let typeConstraints = infer s
            in case mgu typeConstraints of 
                Left err    -> Left err
                Right subst -> 
                    let varnames = DS.toList (getTVars typeConstraints)
                        agg :: TypeEnv -> String -> Either String TypeEnv 
                        agg acc varname = do 
                            ty <- ground varname subst
                            return (DM.insert (Var varname) ty acc)
                    in foldM agg DM.empty varnames