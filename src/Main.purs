module Main where

import Data.Either (Either(..))
import Data.Map (lookup)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console as Console
import Effect.Ref (Ref, new, read, write)
import Infer (TypeEnv(..), inferExpr, inferTop)
import Node.ReadLine (prompt, close, setLineHandler, setPrompt, noCompletion, createConsoleInterface)
import Parser (stringToExpr)
import Prelude (Unit, bind, discard, pure, show, ($), (<>), (==))
import Syntax (Expr)
import Type (RType(..), Scheme(..), TVar(..), Type(..), typeBool, typeInt)

infer :: Expr -> String
infer x = case ( inferExpr $ TypeEnv $ Map.fromFoldable [ Tuple "x" (Forall [] typeInt)] ) x of
  Left _ -> "error"
  Right rx -> show rx

parser :: Ref TypeEnv -> String -> Effect String
parser tpv s = do
  case stringToExpr s of
    Left x -> pure $ "Parse error: " <> x
    Right (Tuple mvn expr) -> do
      tp <- read tpv
      case mvn of
        Nothing -> 
          case inferExpr tp expr of
            Left er -> pure $ show er
            Right sch -> pure $ show sch
        Just vn -> 
          case inferTop tp [Tuple vn expr] of
            Left er -> pure $ show er
            Right env -> do
              write env tpv
              let TypeEnv mp = env
              case lookup vn mp of
                Just sch -> pure $ vn <> ": " <> show sch
                Nothing -> pure "bug"


main :: Effect Unit
main = do
  tpv <- new $ TypeEnv $ Map.fromFoldable 
    [
      Tuple "{}" $ Forall [] (TRec (RNil)),
      Tuple "true" $ Forall [] typeBool,
      Tuple "false" $ Forall [] typeBool,
      Tuple "plus" $ Forall [] (TArr typeInt (TArr typeInt typeInt)),  
      Tuple "isEqual" $ Forall [TV "a"] (TArr (TVar $ TV "a") (TArr (TVar $ TV "a") typeBool)),  
      Tuple "and" $ Forall [] (TArr typeBool (TArr typeBool typeBool))
    ]
  interface <- createConsoleInterface noCompletion
  setPrompt "> " 2 interface
  prompt interface
  setLineHandler interface $ \s ->
    if s == "quit"
       then close interface
       else do
        res <- parser tpv s
        Console.log $ res
        prompt interface
 