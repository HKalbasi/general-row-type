module Infer where

import Control.Monad.Error.Class (try)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.State (State, evalState, get, put)
import Data.Array (foldM, length, nub, range, uncons, zip)
import Data.Either (Either(..))
import Data.Foldable (foldr)
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Data.Tuple as Tuple
import Prelude (class Show, show, (<>), ($), map, (<<<), bind, discard, pure, (==), otherwise, class Semigroup, (+))
import Syntax (Binop(..), Expr(..), Lit(..), Recop(..), Var)
import Type (FST(..), RType(..), Scheme(..), TVar(..), Type(..), typeBool, typeInt)

newtype TypeEnv = TypeEnv (Map.Map Var Scheme)

type Unique = { count :: Int }

data TypeError
  = UnificationFail Type Type
  | InfiniteType TVar Type
  | UnboundVariable String
  | RowMono TVar Type

instance showTypeError :: Show TypeError where
  show (UnificationFail x y) = "can not match\n    " <> show x <> "\nwith\n    " <> show y
  show (InfiniteType v t) = "infinite type in " <> show v <> " = " <> show t
  show (UnboundVariable s) = show s <> " is not defined"
  show (RowMono v t) = "Type variable " <> show v <>
    " is mono and row at the same time in:\n    " <> show t

data EitFTR = Ect Type | Ecf FST | Ecr RType

type Infer = ExceptT TypeError (State Unique)
type Subst = Map.Map TVar (EitFTR)

runInfer :: Infer (Tuple Subst Type) -> Either TypeError Scheme
runInfer m = case evalState (runExceptT m) initUnique of
  Left err  -> Left err
  Right res -> Right $ closeOver res

closeOver :: Tuple Subst (Type) -> Scheme
closeOver (Tuple sub ty) = normalize sc
  where sc = generalize emptyTyenv (apply sub ty)

initUnique :: Unique
initUnique = { count : 0 }

extend :: TypeEnv -> (Tuple Var Scheme) -> TypeEnv
extend (TypeEnv env) (Tuple x s) = TypeEnv $ Map.insert x s env

emptyTyenv :: TypeEnv
emptyTyenv = TypeEnv Map.empty

typeof :: TypeEnv -> Var -> Maybe Scheme
typeof (TypeEnv env) name = Map.lookup name env

class Substitutable a where
  apply :: Subst -> a -> a
  ftv   :: a -> Set.Set TVar

instance substFST :: Substitutable FST where
  apply s Absent = Absent
  apply s (Present t) = Present $ apply s t
  apply s t@(FVar a)       = case Map.lookup a s of
    Nothing        -> t
    Just (Ecf x)  -> x
    Just (_) -> FVar $ TV "there is some problem"
  
  ftv Absent = Set.empty
  ftv (Present t) = ftv t
  ftv (FVar a) = Set.singleton a

instance substRType :: Substitutable RType where
  apply s RNil           = RNil
  apply s (RCons l t rm) = RCons l (apply s t) (apply s rm)
  apply s t@(RVar a)       = case Map.lookup a s of
    Nothing        -> t
    Just (Ecr x)  -> x
    Just (_) -> RVar $ TV "there is some problem"
    
  ftv RNil       = Set.empty
  ftv (RCons l t rm) = ftv t `Set.union` ftv rm
  ftv (RVar a) = Set.singleton a

instance substType :: Substitutable Type where
  apply _ (TCon a)       = TCon a
  apply s t@(TVar a)     = case Map.lookup a s of
    Nothing -> t
    Just (Ect x)  -> x
    Just (_) -> TVar $ TV "there is some problem"
  apply s (TArr t1 t2) = apply s t1 `TArr` apply s t2
  apply s (TRec x) = TRec (apply s x) 

  ftv (TCon _)         = Set.empty
  ftv (TVar a)       = Set.singleton a
  ftv (TArr t1 t2) = ftv t1 `Set.union` ftv t2
  ftv (TRec x) = ftv x 

instance substScheme :: Substitutable Scheme where
  apply s (Forall as t)   = Forall as $ apply s' t
                            where s' = foldr Map.delete s as
  ftv (Forall as t) = ftv t `Set.difference` Set.fromFoldable as

instance substArray :: Substitutable a => Substitutable (Array a) where
  apply = map <<< apply
  ftv   = foldr (Set.union <<< ftv) Set.empty

instance substEitFTR :: Substitutable EitFTR where
  apply s (Ect x) = Ect $ apply s x
  apply s (Ecf x) = Ecf $ apply s x
  apply s (Ecr x) = Ecr $ apply s x
  
  ftv  (Ect x) = ftv x
  ftv  (Ecf x) = ftv x
  ftv  (Ecr x) = ftv x

instance substTypeEnv :: Substitutable TypeEnv where
  apply s (TypeEnv env) =  TypeEnv $ map (apply s) env
  ftv (TypeEnv env) = ftv $ ( ( List.toUnfoldable $ Map.values env ) :: Array _)


nullSubst :: Subst
nullSubst = Map.empty

compose :: Subst -> Subst -> Subst
compose s1 s2 = map (apply s1) s2 `Map.union` s1

unify ::  Type -> Type -> Infer Subst
unify (TArr l r) (TArr l' r')  = do
  s1 <- unify l l'
  s2 <- unify (apply s1 r) (apply s1 r')
  pure (s2 `compose` s1)

unify (TVar a) t = bindX a t
unify t (TVar a) = bindX a t
unify (TCon a) (TCon b) | a == b = pure nullSubst
unify t1@(TRec abr1) t2@(TRec abr2) = result
  where
    result = 
      let
        removeAb (RNil) = RNil
        removeAb r@(RVar _) = r
        removeAb (RCons l Absent rm) = removeAb rm
        removeAb (RCons l t rm) = RCons l t $ removeAb rm

        r1 = removeAb abr1
        r2 = removeAb abr2

        er = throwError $ UnificationFail t1 t2
        findLabel l r = case r of
          RNil -> throwError $ UnificationFail t1 t2
          RVar _ -> throwError $ UnificationFail t1 t2
          RCons l' t rs -> if l == l' then
            pure $ Tuple t rs
          else do 
            Tuple z rm <- findLabel l rs
            pure $ Tuple z (RCons l' t rm)
        findEndVar r = case r of
          RNil -> Nothing
          RVar x -> Just x
          RCons _ _ x -> findEndVar x
        replaceEnd r th = case r of
          RNil -> th
          RVar x -> th
          RCons a b x -> RCons a b (replaceEnd x th)
      in
        case r1,r2 of
          RNil,RNil -> pure nullSubst
          RVar a , t -> bindY a t
          t , RVar a -> bindY a t
          RCons l z1 rm1 , _ -> do
            ei <- try $ findLabel l r2
            case ei of
              Left _ -> case findEndVar r2 of
                Nothing -> case z1 of
                  Absent -> unify (TRec rm1) (TRec r2)
                  FVar v -> do
                    let sa = Map.singleton v (Ecf Absent)
                    sb <- unify (TRec $ apply sa rm1) (TRec $ apply sa r2)
                    pure (sb `compose` sa)
                  Present _ -> er
                Just v -> do
                  v' <- freshV
                  let sa = Map.singleton v (Ecr $ RCons l z1 (RVar v'))
                  let rm2 = replaceEnd r2 (RVar v')
                  sb <- unify (TRec $ apply sa rm1) (TRec $ apply sa rm2)
                  pure (sb `compose` sa)
              Right (Tuple z2 rm2) -> do
                sa <- case z1,z2 of
                  FVar v,x -> bindZ v x
                  x,FVar v -> bindZ v x
                  Present zm1,Present zm2 -> unify zm1 zm2
                  _,_ -> throwError $ UnificationFail t1 t2 --here should not happen
                sb <- unify (TRec $ apply sa rm1) (TRec $ apply sa rm2)
                pure $ sb `compose` sa
          _ , _ -> er
unify t1 t2 = throwError $ UnificationFail t1 t2

bindX ::  TVar -> Type -> Infer Subst
bindX a t
  | t == TVar a     = pure nullSubst
  | occursCheck a t = throwError $ InfiniteType a t
  | otherwise       = pure $ Map.singleton a (Ect t)

bindY ::  TVar -> RType -> Infer Subst
bindY a t
  | t == RVar a     = pure nullSubst
  | occursCheck a t = throwError $ InfiniteType a (TRec t)
  | otherwise       = pure $ Map.singleton a (Ecr t)

bindZ ::  TVar -> FST -> Infer Subst
bindZ a t
  | t == FVar a     = pure nullSubst
  | occursCheck a t = throwError $ InfiniteType a (TRec RNil) --TODO:here is not good
  | otherwise       = pure $ Map.singleton a (Ecf t)


occursCheck ::  forall a. Substitutable a => TVar -> a -> Boolean
occursCheck a t = a `Set.member` ftv t

freshV :: Infer TVar
freshV = do
  s <- get
  put s{count = s.count + 1}
  pure $ TV ("t" <> show s.count)

fresh :: Infer Type
fresh = do
  v <- freshV
  pure $ TVar v

data Vst = IsV | IsRV | IsFV | NoInfo | Both
instance showVst :: Show Vst where
  show Both = "Both"
  show IsV = "IsV"
  show IsRV = "IsRV"
  show IsFV = "IsFV"
  show NoInfo = "NoInfo"

instance semigroupVst :: Semigroup Vst where
  append NoInfo x = x
  append x NoInfo = x
  append Both x   = Both
  append x Both   = Both
  append IsV IsRV = Both
  append IsRV IsV = Both
  append IsV IsFV = Both
  append IsFV IsV = Both
  append IsFV IsRV = Both
  append IsRV IsFV = Both
  append x _      = x

decideR :: TVar -> RType -> Vst
decideR v t = res 
  where  
    res = case t of
      RNil   -> NoInfo
      RVar qv -> if qv == v then IsRV else NoInfo
      RCons l Absent y -> decideR v y 
      RCons l (Present z) y -> decideT v z <> decideR v y
      RCons l (FVar z) y -> ( if v == z then IsFV else NoInfo ) <> decideR v y

decideT :: TVar -> Type -> Vst
decideT v t = res
  where 
    res = case t of
      TCon _ -> NoInfo
      TVar qv -> if qv == v then IsV else NoInfo
      TArr x y -> decideT v x <> decideT v y
      TRec x -> decideR v x


instantiate :: Scheme -> Infer Type
instantiate (Forall as t) = 
  let    
    trf :: TVar -> Infer (EitFTR)
    trf x = do
      r <- freshV
      case decideT x t of
        Both -> throwError $ RowMono x t
        IsRV -> pure $ Ecr $ RVar r
        IsFV -> pure $ Ecf $ FVar r
        _ -> pure $ Ect $ TVar r
  in
    do
      as' <- traverse trf as
      let s = Map.fromFoldable $ zip as as'
      pure $ apply s t

generalize :: TypeEnv -> Type -> Scheme
generalize env t  = Forall as t
  where as = Set.toUnfoldable $ ftv t `Set.difference` ftv env

ops :: Binop -> Type
ops Add = typeInt `TArr` typeInt `TArr` typeInt
ops Mul = typeInt `TArr` typeInt `TArr` typeInt
ops Sub = typeInt `TArr` typeInt `TArr` typeInt
ops Eql = typeInt `TArr` typeInt `TArr` typeBool

lookupEnv :: TypeEnv -> Var -> Infer (Tuple Subst Type)
lookupEnv (TypeEnv env) x =
  case Map.lookup x env of
    Nothing -> throwError $ UnboundVariable (show x)
    Just s  -> do t <- instantiate s
                  pure (Tuple nullSubst t)

infer :: TypeEnv -> Expr -> Infer (Tuple Subst Type)
infer env ex = case ex of

  RecOp o s -> do
    t <- f 
    pure $ Tuple nullSubst t
    where 
      f = case o of
        SetField -> do
          a <- freshV
          r <- freshV
          q <- freshV
          pure $ (TArr (TVar a) (TArr 
            (TRec (RCons s (FVar q) (RVar r))) 
            (TRec (RCons s (Present $ TVar a) (RVar r) )) ))

        GetField -> do
          a <- freshV
          r <- freshV
          pure $ (TArr (TRec (RCons s (Present $ TVar a) (RVar r))) (TVar a) )

        DelField -> do
          a <- freshV
          r <- freshV
          q <- freshV
          pure $ (TArr 
            (TRec (RCons s (FVar q) (RVar r))) 
            ( TRec $ RVar r ) )


  Var x -> lookupEnv env x

  Lam x e -> do
    tv <- fresh
    let env' = env `extend` Tuple (x) ( Forall [] tv)
    Tuple s1 t1 <- infer env' e
    pure $ Tuple (s1) (apply s1 tv `TArr` t1)

  App e1 e2 -> do
    tv <- fresh
    Tuple s1 t1 <- infer env e1
    Tuple s2 t2 <- infer (apply s1 env) e2
    s3       <- unify (apply s2 t1) (TArr t2 tv)
    pure $ Tuple (s3 `compose` s2 `compose` s1) ( apply s3 tv)

  Let x e1 e2 -> do
    Tuple s1 t1 <- infer env e1
    let env' = apply s1 env
        t'   = generalize env' t1
    Tuple s2 t2 <- infer (env' `extend` (Tuple x t')) e2
    pure $ Tuple (s2 `compose` s1) ( t2)

  If cond tr fl -> do
    tv <- fresh
    inferPrim env [cond, tr, fl] (typeBool `TArr` tv `TArr` tv `TArr` tv)

  Fix e1 -> do
    tv <- fresh
    inferPrim env [e1] ((tv `TArr` tv) `TArr` tv)

  Op op e1 e2 -> do
    inferPrim env [e1, e2] (ops op)

  Lit (LInt _)  -> pure (Tuple nullSubst typeInt)
  Lit (LBool _) -> pure (Tuple nullSubst typeBool)

inferPrim :: TypeEnv -> Array Expr -> Type -> Infer (Tuple Subst Type)
inferPrim env l t = do
  tv <- fresh
  Tuple s1 tf <- foldM inferStep (Tuple nullSubst (\x->x)) l
  s2 <- unify (apply s1 (tf tv)) t
  pure $ Tuple (s2 `compose` s1) ( apply s2 tv)
  where
  inferStep (Tuple s tf) exp = do
    (Tuple s' t) <- infer (apply s env) exp
    pure $ Tuple (s' `compose` s)( tf <<< (TArr t))

inferExpr :: TypeEnv -> Expr -> Either TypeError Scheme
inferExpr env = runInfer <<< infer env

inferTop :: TypeEnv -> Array (Tuple String Expr) -> Either TypeError TypeEnv
{-inferTop env x = Right env
inferTop env ((name, ex):xs) = case inferExpr env ex of
  Left err -> Left err
  Right ty -> inferTop (extend env (name, ty)) xs
-}


inferTop env x = 
  case uncons x of
    Nothing -> Right env
    Just {head : Tuple name ex , tail : xs} ->
      case inferExpr env ex of
        Left err -> Left err
        Right ty -> inferTop (extend env (Tuple name ty)) xs

normalize :: Scheme -> Scheme
normalize (Forall ts body) = Forall (map Tuple.snd ord) (normtype body)
  where
    fvb = (nub $ fv body)
    ord = zip fvb (map TV $ map (\l -> "t" <> show l ) (range 0 (length fvb) ) )

    fv (TVar a)              = [a]
    fv (TArr a b)            = fv a <> fv b
    fv (TCon _)              = []
    fv (TRec RNil)           = []
    fv (TRec (RCons l (Present t) rm)) = fv t <> fv (TRec rm)
    fv (TRec (RCons l (Absent) rm)) = fv (TRec rm)
    fv (TRec (RCons l (FVar a) rm)) = [a] <> fv (TRec rm)
    fv (TRec (RVar a))       = [a] 

    normtype (TRec x) = TRec $ f x
      where
        g a = case Tuple.lookup a ord of
            Just z -> z
            Nothing -> TV "type variable not in signature" 
        f (RNil) = RNil
        f (RCons l (Present t) rm) = RCons l (Present $ normtype t) (f rm)
        f (RCons l (Absent) rm) = RCons l (Absent) (f rm)
        f (RCons l (FVar v) rm) = RCons l (FVar $ g v) (f rm)
        f (RVar a)   = RVar $ g a
          
    normtype (TArr a b) = TArr (normtype a) (normtype b)
    normtype (TCon a)   = TCon a
    normtype (TVar a)   =
      case Tuple.lookup a ord of
        Just x -> TVar x
        Nothing -> TCon "type variable not in signature"
---}
