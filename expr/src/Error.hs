module Error (Error(..)) where
import Expr

data Error = ZeroDivision Expr
           | NegativeNumberSqrt Expr
           | UndefinedVar String
           | PreviouslyDefinedVar String Double

instance Show Error where
  show (ZeroDivision expr) = "Division by zero error in the expression " ++ show expr
  show (NegativeNumberSqrt expr) = "Taking square root from a negative number error in the expression" ++ show expr
  show (UndefinedVar var) = "Undefined variable " ++ var
  show (PreviouslyDefinedVar var value) = "Variable " ++ var ++ " defined previously as " ++ show value

instance Eq Error where
  (ZeroDivision a) == (ZeroDivision b) = a == b
  (NegativeNumberSqrt a) == (NegativeNumberSqrt b) = a == b
  (UndefinedVar var1) == (UndefinedVar var2) = var1 == var2
  (PreviouslyDefinedVar var1 expr1) == (PreviouslyDefinedVar var2 expr2) = (var1 == var2) && (expr1 == expr2)
  _ == _ = False
