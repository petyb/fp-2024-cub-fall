import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.Map as M
import State.MyState
import HW.Eval
import HW.StackMachine

setState :: Stack -> MachineState String
setState ls = MachineState ls (M.empty :: Env String)

testPushNum :: TestTree
testPushNum = testCase "PushNum adds a number to the stack" $ do
  let (finalState, result) = runMyState (execInstr (PushNum 5)) (setState [])
  finalState @?= setState [5]
  result @?= Right ()

testAdd :: TestTree
testAdd = testCase "Add combines top two numbers on the stack" $ do
  let (finalState, result) = runMyState (execInstr Add) (setState [10, 5])
  finalState @?= setState [15]
  result @?= Right ()

testAddStackUnderflow :: TestTree
testAddStackUnderflow = testCase "Add fails with stack underflow" $ do
  let (finalState, result) = runMyState (execInstr Add) (setState [5])
  finalState @?= setState [5]
  result @?= Left (StackUnderflow Add)

testPushVarUndefined :: TestTree
testPushVarUndefined = testCase "PushVar fails for undefined variable" $ do
  let (finalState, result) = runMyState (execInstr (PushVar "x")) (setState [])
  finalState @?= setState []
  result @?= Left (VarUndefined "Undefined variable \"x\"")

testStoreVar :: TestTree
testStoreVar = testCase "StoreVar stores value from stack" $ do
  let (finalState, result) = runMyState (execInstr (StoreVar "x")) (setState [5])
  finalState @?= MachineState [] (M.singleton "x" 5)
  result @?= Right ()

testExecProgramEmpty :: TestTree
testExecProgramEmpty = testCase "Exec program with no instructions returns the stack" $ do
  let result = execProgram [] (setState [42])
  result @?= Right (setState [42])

testExecProgramStackNotExhausted :: TestTree
testExecProgramStackNotExhausted = testCase "Exec program with non-singleton stack fails" $ do
  let result = execProgram []  (setState [1, 2]) :: Either (Error String) (MachineState String)
  result @?= Left (StackNotExhausted [1, 2])

tests :: TestTree
tests = testGroup "HW.Eval Tests"
  [ testPushNum
  , testAdd
  , testAddStackUnderflow
  , testPushVarUndefined
  , testStoreVar
  , testExecProgramEmpty
  , testExecProgramStackNotExhausted
  ]

main :: IO ()
main = defaultMain tests