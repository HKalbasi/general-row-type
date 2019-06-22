module Type where

import Prelude

import Data.Maybe (Maybe(..))

newtype TVar = TV String
derive instance eqTVar :: Eq (TVar)
derive instance ordTVar :: Ord (TVar)
instance showTVar :: Show (TVar)
  where show (TV x) = "'" <> x

data RType = RNil | RCons String (Maybe Type) RType | RVar TVar
instance showRType :: Show (RType)
  where 
    show RNil = ""
    show (RCons s t x) = 
      let
        r1 = case t of
          Just j -> s <> " :: " <> show j
          Nothing -> s <> " ?"
        r2 = case x of
          RCons _ _ _ -> " , " <> show x
          _ -> " " <> show x
      in
        r1 <> r2
    show (RVar x) = "| " <> show x

derive instance   eqRType :: Eq   (RType)
derive instance  ordRType :: Ord  (RType)

data Type
  = TVar TVar
  | TCon String
  | TArr Type Type
  | TRec RType
derive instance   eqType :: Eq   (Type)
derive instance  ordType :: Ord  (Type)
instance showType :: Show (Type)
  where 
    show (TVar x) = show x
    show (TCon x) = x
    show (TArr x y) = "( " <> show x <> " -> " <> show y <> " )"
    show (TRec r) = "{ " <> show r <> " }"
    
data Scheme = Forall (Array TVar) Type
derive instance   eqScheme :: Eq   (Scheme)
derive instance  ordScheme :: Ord  (Scheme)
instance showScheme :: Show (Scheme)
  where 
    show (Forall [] y) = show y
    show (Forall x y) = "Forall " <> show x <> " " <> show y
    
typeInt :: Type
typeInt  = TCon "Int"

typeBool :: Type
typeBool = TCon "Bool"
