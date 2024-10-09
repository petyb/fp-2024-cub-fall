module Expr (Expr(..)) where

data Expr = Num Double
          | Var String
          | Sqrt Expr
          | Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          | Pow Expr Expr
          | Let String Expr Expr
        deriving (Eq)


instance Show Expr where
  show (Num x) = show x
  show (Var v) = v
  show (Sqrt expr) = "sqrt(" ++ show expr ++ ")"
  show (Add expr1 expr2) = "(" ++ show expr1 ++ ") + (" ++ show expr2 ++ ")"
  show (Sub expr1 expr2) = "(" ++ show expr1 ++ ") - (" ++ show expr2 ++ ")"
  show (Mul expr1 expr2) = "(" ++ show expr1 ++ ") * (" ++ show expr2 ++ ")"
  show (Div expr1 expr2) = "(" ++ show expr1 ++ ") / (" ++ show expr2 ++ ")"
  show (Pow expr1 expr2) = "(" ++ show expr1 ++ ") ^ (" ++ show expr2 ++ ")"
  show (Let var expr1 expr2) = "Let " ++ var ++ " = " ++ show expr1 ++ " in " ++ show expr2
