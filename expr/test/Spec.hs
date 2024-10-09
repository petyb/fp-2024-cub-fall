import qualified Data.Map.Strict as M
import Interpreter
import qualified Expr
import qualified Data.Map as Map
import qualified Error
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

testBasicOperations :: TestTree
testBasicOperations =
    testGroup "Basic Operations" [
      testCase "2 == 2" $ eval M.empty (Expr.Num 2) @?= Right 2,
      testCase "sqrt(4) == 2" $ eval M.empty (Expr.Sqrt (Expr.Num 4)) @?= Right 2,
      testCase "2 + 3 == 5" $ eval M.empty (Expr.Add (Expr.Num 2) (Expr.Num 3)) @?= Right 5,
      testCase "2 * 3 == 6" $ eval M.empty (Expr.Mul (Expr.Num 2) (Expr.Num 3)) @?= Right 6,
      testCase "2 - 3 == -1" $ eval M.empty (Expr.Sub (Expr.Num 2) (Expr.Num 3)) @?= Right (-1),
      testCase "2 / 3 == 2 / 3" $ eval M.empty (Expr.Div (Expr.Num 2) (Expr.Num 3)) @?= Right (2/3),
      testCase "2 ^ 3 == 8" $ eval M.empty (Expr.Pow (Expr.Num 2) (Expr.Num 3)) @?= Right 8
    ]

testComplexExpressions :: TestTree
testComplexExpressions =
    testGroup "Complex Expressions" [
      testCase "(2 + 3) * 2 == 10" $ eval M.empty (Expr.Mul (Expr.Add (Expr.Num 2) (Expr.Num 3)) (Expr.Num 2)) @?= Right 10,
      testCase "sqrt(2 ^ 4) - 1 == 3" $ eval M.empty (Expr.Sub (Expr.Sqrt (Expr.Pow (Expr.Num 2) (Expr.Num 4))) (Expr.Num 1)) @?= Right 3
    ]

testErrorsWithoutVariables :: TestTree
testErrorsWithoutVariables =
    testGroup "Errors Without Variables" [
      testCase "2 / 0 - can't divide by zero" $ eval M.empty (Expr.Div (Expr.Num 2) (Expr.Num 0)) @?= Left (Error.ZeroDivision (Expr.Div (Expr.Num 2) (Expr.Num 0) )),
      testCase "sqrt(-1) - can't take square root of a negative number" $ eval M.empty (Expr.Sqrt (Expr.Num (-1))) @?= Left (Error.NegativeNumberSqrt (Expr.Sqrt (Expr.Num (-1))))
    ]

testBasicOperationsWithVariables :: TestTree
testBasicOperationsWithVariables = 
  testGroup "Basic Operations With Variables" [
      testCase "v = 2, if v = 2" $ eval (Map.singleton "v" 2) (Expr.Var "v") @?= Right 2,
      testCase "v + 3 = 5, if v = 2" $ eval (Map.singleton "v" 2) (Expr.Add (Expr.Var "v") (Expr.Num 3)) @?= Right 5,
      testCase "v * 3 = 6, if v = 2" $ eval (Map.singleton "v" 2) (Expr.Mul (Expr.Var "v") (Expr.Num 3)) @?= Right 6,
      testCase "v - 3 = -1, if v = 2" $ eval (Map.singleton "v" 2) (Expr.Sub (Expr.Var "v") (Expr.Num 3)) @?= Right (-1),
      testCase "v / 3 = 2/3, if v = 2" $ eval (Map.singleton "v" 2) (Expr.Div (Expr.Var "v") (Expr.Num 3)) @?= Right (2/3),
      testCase "v ^ 3 = 8, if v = 2" $ eval (Map.singleton "v" 2) (Expr.Pow (Expr.Var "v") (Expr.Num 3)) @?= Right 8
    ]

testComplexExpressionsWIthVariables :: TestTree
testComplexExpressionsWIthVariables =
  testGroup "Complex Expressions With Variables" [
      testCase "(a + 2) * b = 10, if a = 3, b = 2" $ eval (Map.fromList [("a", 3), ("b", 2)]) (Expr.Mul (Expr.Add (Expr.Var "a") (Expr.Num 2)) (Expr.Var "b")) @?= Right 10,
      testCase "(a / b) * c = 1, if a = 3, b = 2, c = 0" $ eval (Map.fromList [("a", 3), ("b", 2), ("c", 0)]) (Expr.Pow (Expr.Div (Expr.Var "a") (Expr.Var "b")) (Expr.Var "c")) @?= Right 1
    ] 

testComplexExpressionsWithLet :: TestTree
testComplexExpressionsWithLet = 
  testGroup "Complex Expressions With Let" [
    testCase "Let x = a + 1 in (x * 2) ^ b should be 9 if a = 0.5, b = 2" $ eval (Map.fromList [("a", 0.5), ("b", 2)]) (Expr.Let "x" (Expr.Add (Expr.Var "a") (Expr.Num 1)) (Expr.Pow (Expr.Mul (Expr.Var "x") (Expr.Num 2)) (Expr.Var "b"))) @?= Right 9,
    testCase "Let x = 1 in Let y = (x + 1) in y * 2 should be 4" $ eval Map.empty (Expr.Let "x" (Expr.Num 1) (Expr.Let "y" (Expr.Add (Expr.Var "x") (Expr.Num 1)) (Expr.Mul (Expr.Var "y") (Expr.Num 2)))) @?= Right 4
  ]

testErrorsWithVariablesAndLet :: TestTree
testErrorsWithVariablesAndLet =
  testGroup "Errors With Variables And Let" [
    testCase "2 + a - where a is undefined" $ eval (Map.singleton "b" 2) (Expr.Add (Expr.Num 2) (Expr.Var "a")) @?= Left (Error.UndefinedVar "a"),
    testCase "Let a = 2 in (a + 2), but there is a = 1 in map" $ eval (Map.singleton "a" 1) (Expr.Let "a" (Expr.Num 2) (Expr.Add (Expr.Var "a") (Expr.Num 2))) @?= Left (Error.PreviouslyDefinedVar "a" 1)
  ]

main :: IO ()
main =
  defaultMain $ testGroup "Expressions" [
    testBasicOperations,
    testComplexExpressions,
    testErrorsWithoutVariables,
    testBasicOperationsWithVariables,
    testComplexExpressionsWIthVariables,
    testComplexExpressionsWithLet,
    testErrorsWithVariablesAndLet
  ]