module StackMachine where

-- Values (elements of the stack)
data Value
  = VInt Int
  | VBool Bool
  deriving (Show, Eq)

-- Unary Operators
data Op1
  = ONeg -- integer negation
  | ONot -- boolean not
  deriving (Show, Eq)

-- Binary Operators
data Op2 
  = OAdd -- integer addition
  | OSub -- integer subtraction
  | OMul -- integer multiplication
  | OLt  -- integer inequality (less than)
  | OAnd -- boolean and
  | OOr  -- boolean or
  deriving (Show, Eq)

-- Instructions
data Instr 
  = IPush Value       -- push the value on the top of the stack.
  | IPop              -- remove the top value of the stack.
  | INoop             -- does nothing (leaves the stack unchanged).
  | IDup              -- duplicates the the top value of stack.
  | IDup2             -- duplicates the top two values of the stack.
  | IFlip             -- swaps the top two values of the stack.
  | IOp1 Op1          -- apply a unary operator to the top of the stack.
  | IOp2 Op2          -- apply a binary operator to the top two values of the stack.
  | IIf Instr Instr   -- if the top value is True then continue with the first instruction
                      -- otherwise continue with the second instruction.
  | ISeq Instr Instr  -- run two instructions in after each other.
  deriving (Show, Eq)

type Stack = [Value]
type Error = String

evalOp1 :: Op1 -> Value -> Either Error Value
evalOp1 ONeg (VInt x)  = Right $ VInt $ -x
evalOp1 ONot (VBool x) = Right $ VBool $ not x
evalOp1 o    _         = Left $ "Invalid value for unary operator " ++ show o

evalOp2 :: Op2 -> Value -> Value -> Either Error Value
evalOp2 OAdd (VInt x)  (VInt y)  = Right $ VInt $ x + y
evalOp2 OSub (VInt x)  (VInt y)  = Right $ VInt $ x - y
evalOp2 OMul (VInt x)  (VInt y)  = Right $ VInt $ x * y
evalOp2 OLt  (VInt x)  (VInt y)  = Right $ VBool $ x < y
evalOp2 OAnd (VBool x) (VBool y) = Right $ VBool $ x && y
evalOp2 OOr  (VBool x) (VBool y) = Right $ VBool $ x || y
evalOp2 o    _         _         = Left $ "Invalid values for binary operator " ++ show o

eval :: Instr -> Stack -> Either Error Stack
eval (IPush v)    s = Right (v:s) 
eval IPop         s = case s of
                        [] -> Left "Cannot use pop on empty stack"
                        (_ : s') -> Right s'
eval INoop        s = Right s
eval IDup         s = case s of
                        [] -> Left "Cannot use dup on empty stack"
                        (x : s) -> Right (x : x : s)
eval IDup2        s = case s of
                        (x : y : s) -> Right (x : y : x : y : s)
                        _ -> Left "Cannot use dup2 on stack with < 2 values"
eval IFlip        s = case s of
                        (x : y : s) -> Right (y : x : s)
                        _ -> Left "Cannot use flip on stack with < 2 values"
eval (IOp1 o)     s = case s of
                        (x : s) -> (:s) <$> evalOp1 o x
                        _ -> Left "Cannot use unary operator on empty stack"
eval (IOp2 o)     s = case s of
                        (x : y : s) -> (:s) <$> evalOp2 o x y
                        _ -> Left "Cannot use binary operator on stack with < 2 values"
eval (IIf i1 i2)  s = case s of
                        (VBool True  : s) -> eval i1 s
                        (VBool False : s) -> eval i2 s
                        _                 -> Left "Cannot use if on stack with no boolean on top"
eval (ISeq i1 i2) s = eval i1 s >>= eval i2

