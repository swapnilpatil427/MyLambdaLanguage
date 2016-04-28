module Eval (Value(..),Op(..),addExpr,subExpr,mulExpr,Expression(..),eval) where

import Debug.Trace
import Data.Map (Map)
import qualified Data.Map as Map
type Variable = String
data Op = Add | Sub | Mul deriving (Eq,Show)

data Expression = Abs Variable Expression
          | App Expression Expression
          | Var Variable
          | Lambda Variable Expression
          | Const Int
          | Ref Expression
          | Binop Op Expression Expression
          | Deref Expression
          | Assign Expression Expression
          | Bottom
          deriving (Eq,Show)

data Value = FunVal Params Variable Expression
         | Val Int
         | Addrss Int
         | Bot

instance Show Value where
  show (FunVal _ _ _) = "<fun>"
  show (Val n) = show n
  show (Addrss n) = show n
  show Bot = error "Not Found"

type Store = Map Int Value
type Params = Map Variable Value


addExpr :: Expression -> Expression -> Expression
addExpr = Binop Add

subExpr :: Expression -> Expression -> Expression
subExpr = Binop Sub

mulExpr :: Expression -> Expression -> Expression
mulExpr = Binop Mul

evalOp :: Op -> (Int -> Int -> Int)
evalOp Add = (+)
evalOp Sub = (-)
evalOp Mul = (*)

evalIn :: Store -> Params -> Expression -> (Store , Value )
evalIn s p (Abs x e) = (s,FunVal p x e)

evalIn s p (App e1 e2) =
    case evalIn s p e1 of  -- Abs "x" (Binop Mul (Var "x") (Var "x"))
    (s1,FunVal p' x e3) ->
      let (s2,v2) = evalIn s1 p e2 in
      let p'' = Map.insert x v2 p' in
      evalIn s2 p'' e3
    (s1,Bot) -> (s2, Bot) where (s2,v2) = evalIn s1 p e2

evalIn s p (Var x) = case (Map.lookup x p) of
            Just v -> (s , v)

evalIn s _ (Const n) =(s, Val n)

--Deref (Assign (Ref (Const 3)) (Const 4))   Assign (Ref (Const 3)) (Const 4)
evalIn s p (Ref e) = do
  case evalIn s p e of
   (s1,v1) ->  let size = Map.size s1 in let s2 = Map.insert (size+1) v1 s1 in (s2,Addrss (size + 1))

evalIn s p (Deref e) = do
  case evalIn s p e of
     (s',Addrss a) -> case (Map.lookup a s') of
                 Nothing -> error "Not Found"
                 Just a -> (s' , a)
     (s' , Bot) -> (s',Bot)
evalIn s p (Assign e1 e2) = do
  case evalIn s p e1 of
      (s1,Addrss a) ->  case evalIn s1 p e2 of
                        (s2,v) -> let s3 = Map.insert a v s2 in (s3,v)
      (s2, Bot) -> let (s3,v) = evalIn s2 p e2 in (s3, Bot)

evalIn s _ Bottom = (s, Bot)

evalIn s p (Binop op e1 e2) =
  let (s1,v1) = evalIn s p e1
      (s2,v2) = evalIn s p e2
      x = evalOp op in
  case (v1,v2) of
    (Val n1,Val n2) ->(s, Val (n1 `x` n2))
    _ -> error "Not a number"

eval :: Expression -> (Store,Value)
eval x = evalIn Map.empty Map.empty x
