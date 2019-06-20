module Parser where

import Prelude
import Syntax
import Type

import Control.Monad.Error.Class (throwError)
import Data.Array (filter, fold, length, slice)
import Data.Array (foldl, (!!))
import Data.Either (Either(..))
import Data.Foldable (class Foldable)
import Data.Int (fromString)
import Data.List (List(..), (:), uncons, fromFoldable)
import Data.Map (empty, lookup)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), split)
import Data.String.CodeUnits (toCharArray)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console as Console
import Node.ReadLine (prompt, close, setLineHandler, setPrompt, noCompletion, createConsoleInterface)


type VarHolder = Map.Map String Expr

defaultVars :: VarHolder
defaultVars = Map.fromFoldable [
    Tuple "0" (Lit (LInt 0)) ,
    Tuple "true" (Lit (LBool true)) 
  ]


simpleFold :: forall x. (x -> x -> x) -> List x -> Maybe x
simpleFold f x = case uncons x of
  Just { head : a , tail : b } -> Just $ foldl f a b
  Nothing -> Nothing



data Tree a x = Leaf x | Tree (a (Tree a x))

instance myTreeMap :: Functor a => Functor (Tree a) where
  map f (Leaf x) = Leaf (f x)
  map f (Tree x) = Tree (map (map f) x)


instance myTreeShow :: ( Show x , Functor a , Foldable a ) => Show (Tree a x) where
  show (Leaf x) = show x
  show (Tree x) = "[ " <> fold (map ((_ <> " , ") <<< show) x) <> " ]"

dfs :: forall a x y. Functor a => ( x -> y ) -> ( a y -> y ) -> Tree a x -> y
dfs f _ (Leaf x) = f x
dfs f1 f (Tree x) = f (map (dfs f1 f) x)  

parseParantez :: Array String -> Either String (Tree List String)
parseParantez sp = 
  let
    listParser :: Array String -> Either String (Tree List String)
    listParser ls = let
      lpi :: Array String -> Int -> Int -> Either String (Tuple Int (Tree List String))
      lpi tape i depth = if i >= length tape then
        if depth == 0 then 
          pure $ Tuple i (Tree Nil)
        else
          throwError "bad parantez"
      else
        case tape !! i of
          Just c -> 
            if (c == "(") then do
              Tuple rsi rsl <- lpi tape (i+1) (depth+1)
              Tuple ri rlt <- lpi tape rsi depth
              rl <- case rlt of
                Tree x -> pure x
                Leaf _ -> throwError "something impossible3;"
              pure $ Tuple ri (Tree (rsl: rl) )
            else if (c == ")") then pure $ Tuple (i+1) (Tree Nil)
            else do
              Tuple ri rlt <- lpi tape (i+1) depth
              rl <- case rlt of
                Tree x -> pure x
                Leaf _ -> throwError "something impossible1;"
              pure $ Tuple ri (Tree (Leaf c : rl) )
          Nothing -> throwError $ "something impossible2;" <> show i
    in
      do
        Tuple i l <- lpi ls 0 0
        if (i /= length ls) then throwError "bad parantez" else pure unit
        pure l
  in
    listParser $ sp


stringToExpr :: String -> Either String (Tuple (Maybe String) Expr) 
stringToExpr s = 
  let
    parseToken :: String -> Either String Expr
    parseToken token = 
      if token == "\\" then Left "Lambda"
      else
        pure $ case fromString token of
          Just i -> Lit $ LInt i 
          Nothing -> case fromFoldable (split (Pattern "") token) of
            "." : xs -> RecOp GetField (fold xs)
            "=" : xs -> RecOp SetField (fold xs)
            "-" : xs -> RecOp DelField (fold xs)
            _ -> Var token 
    merge x y = (App <$> x <*> y)
    myFold :: List (Either String Expr) -> Either String Expr
    myFold x =
      case x of
        Cons (Left "Lambda") xt ->
          case xt of
            Nil -> throwError "Lambda syntax error"
            Cons xh xtt -> do
              pxh <- xh
              pxt <- myFold xtt
              xv <- case pxh of
                Var a -> pure a
                _ -> throwError "Lambda var not found"
              pure $ Lam xv pxt
        _ -> case simpleFold merge x of
          Just a -> a
          Nothing -> throwError "empty query"
    
  in do
    let sp = filter ( _ /= "") $ split (Pattern " ") s
    Tuple vn spe <- case sp !! 1 , sp !! 0 of
      Just "=" , Just x -> pure $ Tuple (Just x) (slice 2 (length sp) sp)
      _,_ -> pure $ Tuple Nothing sp
    tree <- parseParantez $ spe
    exp <- dfs parseToken myFold tree
    pure $ Tuple vn exp