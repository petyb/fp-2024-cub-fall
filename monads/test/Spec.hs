import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.Map as M
import State.MyState
import HW.Compiler
import HW.Eval
import HW.StackMachine 

pushFive :: StackInstr String
pushFive = PushNum 5

addInstr :: StackInstr String
addInstr = Add

testPushNum :: TestTree
testPushNum = testCase "PushNum adds a number to the stack" $ do
  let initialState = MachineState [] M.empty
  let (finalState, result) = runMyState (execInstr pushFive) initialState
  finalState @?= MachineState [5] M.empty
  result @?= Right ()

testAdd :: TestTree
testAdd = testCase "Add combines top two numbers on the stack" $ do
  let initialState = MachineState [10, 5] M.empty
  let (finalState, result) = runMyState (execInstr addInstr) initialState
  finalState @?= MachineState [15] M.empty
  result @?= Right ()

testAddStackUnderflow :: TestTree
testAddStackUnderflow = testCase "Add fails with stack underflow" $ do
  let initialState = MachineState [5] M.empty
  let (finalState, result) = runMyState (execInstr addInstr) initialState
  finalState @?= MachineState [5] M.empty
  result @?= Left (StackUnderflow Add)

testPushVarUndefined :: TestTree
testPushVarUndefined = testCase "PushVar fails for undefined variable" $ do
  let initialState = MachineState [] (M.empty :: Env String)
  let (finalState, result) = runMyState (execInstr (PushVar "x")) initialState
  finalState @?= initialState
  result @?= Left (VarUndefined "Undefined variable \"x\"")

testStoreVar :: TestTree
testStoreVar = testCase "StoreVar stores value from stack" $ do
  let initialState = MachineState [5] (M.empty :: Env String)
  let (finalState, result) = runMyState (execInstr (StoreVar "x")) initialState
  finalState @?= MachineState [] (M.singleton "x" 5)
  result @?= Right ()

testExecProgramEmpty :: TestTree
testExecProgramEmpty = testCase "Exec program with no instructions returns the stack" $ do
  let initialState :: MachineState String
      initialState = MachineState [42] M.empty
  let result :: Either (Error String) (MachineState String)
      result = execProgram [] initialState
  result @?= Right initialState

testExecProgramStackNotExhausted :: TestTree
testExecProgramStackNotExhausted = testCase "Exec program with non-singleton stack fails" $ do
  let initialState = MachineState [1, 2] M.empty :: MachineState String
  let result = execProgram [] initialState :: Either (Error String) (MachineState String)
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