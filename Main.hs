module Main where

import Control.Monad (unless)
import Text.Printf (printf)
import Distribution.Compat.ResponseFile (expandResponse)
import Text.XHtml (base, abbr, reset)

data Expr = Num Double
          | Sqrt Expr
          | Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          | Pow Expr Expr


instance Show Expr where
  show (Num x) = show x
  show (Sqrt expr) = "sqrt(" ++ show expr ++ ")"
  show (Add expr1 expr2) = "(" ++ show expr1 ++ ") + (" ++ show expr2 ++ ")"
  show (Sub expr1 expr2) = "(" ++ show expr1 ++ ") - (" ++ show expr2 ++ ")"
  show (Mul expr1 expr2) = "(" ++ show expr1 ++ ") * (" ++ show expr2 ++ ")"
  show (Div expr1 expr2) = "(" ++ show expr1 ++ ") / (" ++ show expr2 ++ ")"
  show (Pow expr1 expr2) = "(" ++ show expr1 ++ ") ^ (" ++ show expr2 ++ ")"

instance Eq Expr where
  a == b = show a == show b


data Error = ZeroDivision
           | NegativeNumberSqrt

instance Show Error where
  show :: Error -> String
  show ZeroDivision = "Division by zero error"
  show NegativeNumberSqrt = "Taking square root from a negative number error"


instance Eq Error where
  ZeroDivision == ZeroDivision = True
  NegativeNumberSqrt == NegativeNumberSqrt = True
  _ == _ = False

eval :: Expr -> Either Error Double
eval (Num a) = Right a
eval (Sqrt exp) = let res = eval exp in case res of
  Right x -> if x >= 0 then Right (sqrt x) else Left NegativeNumberSqrt
  Left x -> Left x
eval (Add exp1 exp2) = let
                  a = eval exp1
                  b = eval exp2
                in case (a, b) of
                  (Right a, Right b) -> Right (a + b)
                  (Left a, _) -> Left a
                  (_, Left b) -> Left b
eval (Sub exp1 exp2) = let
                  a = eval exp1
                  b = eval exp2
                in case (a, b) of
                  (Right a, Right b) -> Right (a - b)
                  (Left a, _) -> Left a
                  (_, Left b) -> Left b
eval (Mul exp1 exp2) = let
                  a = eval exp1
                  b = eval exp2
                in case (a, b) of
                  (Right a, Right b) -> Right (a * b)
                  (Left a, _) -> Left a
                  (_, Left b) -> Left b
eval (Div exp1 exp2) = let
                  a = eval exp1
                  b = eval exp2
                in case (a, b) of
                  (Right a, Right b) -> if b == 0 then Left ZeroDivision else Right (a / b)
                  (Left a, _) -> Left a
                  (_, Left b) -> Left b
eval (Pow exp1 exp2) = let
                  a = eval exp1
                  b = eval exp2
                in case (a, b) of
                  (Right a, Right b) -> Right (a ** b)
                  (Left a, _) -> Left a
                  (_, Left b) -> Left b

cases :: [(Expr, Either Error Double)]
cases = [
    -- basic operations 
    (Num 2.0, Right 2.0),
    (Sqrt (Num 9.0), Right 3.0),
    (Add (Num 2.0) (Num 3.0), Right 5.0),
    (Sub (Num 2.0) (Num 3.0), Right (-1.0)),
    (Mul (Num 2.0) (Num 3.0), Right 6.0),
    (Div (Num 2.0) (Num 3.0), Right (2.0/3)),
    (Pow (Num 2.0) (Num 3.0), Right 8.0),

    -- long expression
    (Sub (Add (Num 2.0) (Div (Num 9.0) (Num 3.0))) (Sqrt (Mul (Num 2.0) (Num 8.0))), Right 1.0),

    (Add (Div (Num 2.0) (Num 0.0)) (Num 3.0), Left ZeroDivision), -- left error
    (Add (Num 3.0) (Div (Num 2.0) (Num 0.0)), Left ZeroDivision), -- right error
    (Add (Div (Num 2.0) (Num 0.0)) (Sqrt(Num (-1.0))), Left ZeroDivision), -- two errors
    (Add (Sqrt(Num (-1.0))) (Div (Num 2.0) (Num 0.0)), Left NegativeNumberSqrt) -- two errors
  ]

test :: Expr -> Either Error Double -> IO ()
test expr expected =
    let actual = eval expr in
    unless (expected == actual) $ describeFailure actual
  where
    describeFailure actual =
      printf "eval (%s) should be %s but it was %s" (show expr) (show expected) (show actual)

main :: IO ()
main = do
  mapM_ (uncurry test) cases
  putStrLn "Done"