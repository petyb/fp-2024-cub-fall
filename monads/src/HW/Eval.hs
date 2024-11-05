module HW.Eval where

import qualified Data.Map as M
import State.MyState
import HW.StackMachine

-- Possible errors during evaluation
data Error v
  = StackUnderflow (StackInstr v) -- Not enough numbers on the stack to process the instruction
  | VarUndefined String           -- The variable is not defined in the environment 
  | StackNotExhausted Stack       -- After the program has finished evaluation, the stack is not a singleton
  deriving (Show, Eq)

type Stack = [Int] -- The stack, holding integers

type Env v = M.Map v Int -- The environment, mapping variables to their values

-- The machine state consists of the stack and the variable environment.
data MachineState v = MachineState
  { getStack :: Stack,
    getEnv :: Env v
  }
  deriving (Show, Eq)

-- Run the compiled program on an empty stack and environment
initialState :: MachineState String
initialState = MachineState [] M.empty

-- Execute a single instruction. 
-- Successful evaluation does not produce any useful result: only the effect of modifying state matters. 
execInstr :: (Ord v, Show v) => StackInstr v -> MyState (MachineState v) (Either (Error v) ())
execInstr (PushNum x) = fmap (const (Right ())) (modify (\s -> s { getStack = x : getStack s }))

execInstr (PushVar v) = do
  state <- get
  let env = getEnv state
  case M.lookup v env of
    Nothing -> return (Left (VarUndefined ("Undefined variable " ++ show v)))
    Just val -> fmap (const (Right ())) (modify (\s -> s { getStack = val : getStack state }))

execInstr Add = do
  state <- get
  let stack = getStack state
  case stack of
    (x:y:xs) -> fmap (const (Right ())) (modify (\s -> s { getStack = (x + y) : xs }))
    _ -> return (Left (StackUnderflow Add))

execInstr (StoreVar var) = do
  state <- get
  let stack = getStack state
  case stack of 
    (x:xs) -> do
      put (state {getStack = xs, getEnv = M.insert var x (getEnv state)})
      return (Right ())
    _ -> return (Left (StackUnderflow (StoreVar var)))

-- Execute a list of instructions starting from the given state. 
execProgram :: (Ord v, Show v) => StackProgram v -> MachineState v -> Either (Error v) (MachineState v)
execProgram [] state = let stack = getStack state in case stack of
  [_] -> Right state
  _ -> Left (StackNotExhausted stack)
execProgram (x:xs) state = let res = execInstr x in case runMyState res state of 
  (_, Left err) -> Left err
  (state', Right ()) -> execProgram xs state'
