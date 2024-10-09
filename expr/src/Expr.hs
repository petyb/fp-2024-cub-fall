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

-- eval :: M.Map String Expr -> Expr -> Maybe Int
-- eval _ (Lit n) = Just n
-- eval state (Plus x y) =
--   case (eval state x, eval state y) of
--     (Just x, Just y) -> Just $ x + y
--     _ -> Nothing
-- eval state (Var v) = do
--   case M.lookup v state of
--     Just v -> eval state v
--     Nothing -> Nothing

-- run :: Expr -> M.Map String Expr -> IO ()
-- run expr state = do
--   print expr
--   print state
--   print (eval state expr)
--   putStrLn ""

-- main = do
--   let expr1 = Var "x"
--   let expr2 = Plus (Lit 2) (Lit 2)
--   let expr3 = Plus (Var "x") (Lit 1)
--   let state1 = M.fromList [("x", Lit 42), ("y", Lit 13)]
--   let state2 = M.empty
--   run expr1 state1
--   run expr2 state1
--   run expr3 state1
--   run expr1 state2
--   run expr2 state2
--   run expr3 state2
