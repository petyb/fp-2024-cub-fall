module Interpreter (eval) where

import Expr
import Error
import qualified Data.Map as Map
import Data.Maybe

eval1 :: Map.Map String Double -> (Double -> Double -> Double) -> Expr -> Expr -> Either Error Double
eval1 mp f exp1 exp2 = let
                  a = eval mp exp1
                  b = eval mp exp2
                in case (a, b) of
                  (Right a, Right b) -> Right (f a b)
                  (Left a, _) -> Left a
                  (_, Left b) -> Left b

eval :: Map.Map String Double -> Expr -> Either Error Double
eval _ (Num a) = Right a
eval mp (Sqrt exp) = let res = eval mp exp in case res of
  Right x -> if x >= 0 then Right (sqrt x) else Left (NegativeNumberSqrt (Sqrt exp))
  Left x -> Left x
eval mp (Add exp1 exp2) = eval1 mp (+) exp1 exp2
eval mp (Sub exp1 exp2) = eval1 mp (-) exp1 exp2
eval mp (Mul exp1 exp2) = eval1 mp (*) exp1 exp2
eval mp (Div exp1 exp2) = if eval mp exp2 == Right 0 then Left (ZeroDivision (Div exp1 exp2)) else eval1 mp (/) exp1 exp2
eval mp (Pow exp1 exp2) = eval1 mp (**) exp1 exp2
eval mp (Var var) = case Map.lookup var mp of
                        Just value -> Right value
                        Nothing -> Left (UndefinedVar var)
eval mp (Let var exp1 exp2) = case Map.lookup var mp of
                                Just value -> Left (PreviouslyDefinedVar var value)
                                Nothing -> case eval mp exp1 of
                                    Right value1 -> eval (Map.insert var value1 mp) exp2
                                    Left err -> Left err
