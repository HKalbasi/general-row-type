module Infer where

import Prelude

import Control.Monad.Error.Class (try)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.State (State, evalState, get, put)
import Data.Array (foldM, length, nub, range, uncons, zip)
import Data.Either (Either(..))
import Data.Foldable (foldr)
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Set as Set
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Data.Tuple as Tuple
import Syntax (Binop(..), Expr(..), Lit(..), Recop(..), Var)
import Type (RType(..), Scheme(..), TVar(..), Type(..), typeBool, typeInt)

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

type Infer = ExceptT TypeError (State Unique)
type Subst = Map.Map TVar (Either RType Type)

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

instance substRType :: Substitutable RType where
  apply s RNil           = RNil
  apply s (RCons l t rm) = RCons l (apply s <$> t) (apply s rm)
  apply s t@(RVar a)       = case Map.lookup a s of
    Just (Right _) -> RVar $ TV "there is some problem"
    Nothing        -> t
    Just (Left x)  -> x

  ftv RNil       = Set.empty
  ftv (RCons l (Just t) rm) = ftv t `Set.union` ftv rm
  ftv (RCons l (Nothing) rm) = ftv rm
  ftv (RVar a) = Set.singleton a

instance substType :: Substitutable Type where
  apply _ (TCon a)       = TCon a
  apply s t@(TVar a)     = case Map.lookup a s of
    Just (Left _)  -> TVar $ TV "there is some problem"
    Nothing        -> t
    Just (Right x) -> x
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

instance substEither :: (Substitutable a , Substitutable b) => Substitutable (Either a b) where
  apply = map <<< apply
  ftv   = foldr (Set.union <<< ftv) Set.empty

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
unify t1@(TRec r1) t2@(TRec r2) = result
  where
    result = 
      let
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
                  Nothing -> unify (TRec rm1) (TRec r2)
                  Just _ -> er
                Just v -> do
                  v' <- freshV
                  let sa = Map.singleton v (Left $ RCons l z1 (RVar v'))
                  let rm2 = replaceEnd r2 (RVar v')
                  sb <- unify (TRec rm1) (TRec rm2)
                  pure (sb `compose` sa)
              Right (Tuple z2 rm2) -> do
                sa <- case z1,z2 of
                  Just zm1,Just zm2 -> unify zm1 zm2
                  _,_ -> pure $ Map.empty
                sb <- unify (TRec $ apply sa rm1) (TRec $ apply sa rm2)
                pure $ sb `compose` sa
          _ , _ -> er
unify t1 t2 = throwError $ UnificationFail t1 t2

bindX ::  TVar -> Type -> Infer Subst
bindX a t
  | t == TVar a     = pure nullSubst
  | occursCheck a t = throwError $ InfiniteType a t
  | otherwise       = pure $ Map.singleton a (Right t)

bindY ::  TVar -> RType -> Infer Subst
bindY a t
  | t == RVar a     = pure nullSubst
  | occursCheck a t = throwError $ InfiniteType a (TRec t)
  | otherwise       = pure $ Map.singleton a (Left t)

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

data Vst = IsV | IsRV | NoInfo | Both
instance semigroupVst :: Semigroup Vst where
  append NoInfo x = x
  append x NoInfo = x
  append Both x   = Both
  append x Both   = Both
  append IsV IsRV = Both
  append IsRV IsV = Both
  append x _      = x

decideR :: TVar -> RType -> Vst
decideR v t =  
    case t of
      RNil   -> NoInfo
      RVar qv -> if qv == v then IsRV else NoInfo
      RCons l x y -> ( maybe NoInfo (\m -> decideT v m) x ) <> decideR v y 

decideT :: TVar -> Type -> Vst
decideT v t = case t of
  TCon _ -> NoInfo
  TVar qv -> if qv == v then IsV else NoInfo
  TArr x y -> decideT v x <> decideT v y
  TRec x -> decideR v x


instantiate ::  Scheme -> Infer Type
instantiate (Forall as t) = 
  let    
    trf :: TVar -> Infer (Either RType Type)
    trf x = do
      r <- freshV
      case decideT x t of
        Both -> throwError $ RowMono x t
        IsRV -> pure $ Left $ RVar r
        _ -> pure $ Right $ TVar r
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
          pure $ (TArr (TVar a) (TArr 
            (TRec (RCons s (Nothing) (RVar r))) 
            (TRec (RCons s (Just $ TVar a) (RVar r) )) ))

        GetField -> do
          a <- freshV
          r <- freshV
          pure $ (TArr (TRec (RCons s (Just $ TVar a) (RVar r))) (TVar a) )

        DelField -> do
          a <- freshV
          r <- freshV
          pure $ (TArr 
            (TRec (RCons s (Nothing) (RVar r))) 
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
    fv (TRec (RCons l (Just t) rm)) = fv t <> fv (TRec rm)
    fv (TRec (RCons l (Nothing) rm)) = fv (TRec rm)
    fv (TRec (RVar a))       = [a] 

    normtype (TRec x) = TRec $ f x
      where
        f (RNil) = RNil
        f (RCons l (Just t) rm) = RCons l (Just $ normtype t) (f rm)
        f (RCons l (Nothing) rm) = RCons l (Nothing) (f rm)
        f (RVar a)   =
          case Tuple.lookup a ord of
            Just x -> RVar x
            Nothing -> RVar $ TV "type variable not in signature"
    normtype (TArr a b) = TArr (normtype a) (normtype b)
    normtype (TCon a)   = TCon a
    normtype (TVar a)   =
      case Tuple.lookup a ord of
        Just x -> TVar x
        Nothing -> TCon "type variable not in signature"
---}
