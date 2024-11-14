
module Main where

import HW.Compiler
import HW.Eval
import HW.StackMachine
import HW.Main
import Expr

import Test.Tasty
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty.Hedgehog (testProperty)

import qualified Data.Set as Set
import Data.Either (isRight)

genNum :: Gen (Expr String)
genNum = Expr.Num <$> Gen.int (Range.constant (-10) 10)

genVar :: Gen String
genVar = Gen.string (Range.constant 1 5) Gen.alpha

genPlus :: Gen (Expr String)
genPlus = Expr.Plus <$> genExpr <*> genExpr

genLet :: Gen (Expr String)
genLet = Expr.Let <$> genVar <*> genExpr <*> genExpr

genExpr :: Gen (Expr String)
genExpr = Gen.recursive Gen.choice [ genNum, Expr.Var <$> genVar ] [ genPlus, genLet ]

plusCount (Plus a b) = 1 + plusCount a + plusCount b
plusCount (Let x a b) = plusCount a + plusCount b
plusCount _ = 0

prop_amount_of_plus_is_the_same :: Property
prop_amount_of_plus_is_the_same = property $ do
  expr <- forAll genExpr
  let compiled = compile expr
  assert (length (filter (== Add) compiled) == plusCount expr)

getNumsFromExpr (Num x) = Set.singleton x
getNumsFromExpr (Plus a b) = Set.union (getNumsFromExpr a) (getNumsFromExpr b)
getNumsFromExpr (Let x a b) = Set.union (getNumsFromExpr a) (getNumsFromExpr b)
getNumsFromExpr _ = Set.empty

getNumsFromStackProgram [] = Set.empty
getNumsFromStackProgram (PushNum x:t) = Set.union (Set.singleton x) (getNumsFromStackProgram t)
getNumsFromStackProgram (h:t) = getNumsFromStackProgram t

prop_set_of_numbers_is_the_same :: Property
prop_set_of_numbers_is_the_same = property $ do
  expr <- forAll genExpr
  let compiled = compile expr
  assert (getNumsFromExpr expr == getNumsFromStackProgram compiled)

compilerTests = 
  [ 
    testProperty "Amount of additions should be the same" prop_amount_of_plus_is_the_same,
    testProperty "The set of numbers used should be the same" prop_set_of_numbers_is_the_same
  ]

prop_one_element_after_correct_evaluation :: Property
prop_one_element_after_correct_evaluation = property $ do
  expr <- forAll genExpr
  let compiled = compile expr
  let executed = execProgram compiled initialState
  case executed of
    Right res -> case getStack res of
      [x] -> success
      _ -> failure
    Left _ -> success

prop_stack_underflow_when_not_enough_elements :: Property
prop_stack_underflow_when_not_enough_elements = property $ do
  expr <- forAll genExpr
  let compiled = (compile expr) ++ [HW.StackMachine.Add]
  let executed = execProgram compiled initialState
  case executed of
    Right res -> failure
    Left _ -> success

interpreterTests =
  [
    testProperty "In case of correct evaluation only 1 element left on stack" prop_one_element_after_correct_evaluation,
    testProperty "When not enough elements on the stack the StackUnderflow error should occur" prop_stack_underflow_when_not_enough_elements
  ]

main :: IO ()
main = defaultMain $ testGroup "All tests" [
      testGroup "Compiler tests" compilerTests,
      testGroup "Interpreter tests" interpreterTests
    ]
