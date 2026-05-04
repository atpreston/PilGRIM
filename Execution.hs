module Execution where

import Clash.Prelude hiding (Word)
import Processor
import Instructions
import Nodes
import Stack
import Continuations

import Data.List((++))

getoutput :: Either String (Mode, Processor) -> Either String Node
getoutput (Left err) = Left err
getoutput (Right (Exit, processor)) = case (peekS s) of
    Just sframe -> case (peekS . nodestack $ sframe) of 
        Just nodebits -> Right $ decodenode nodebits
        Nothing -> Left "Empty Stack Frame, unrecoverable error"
    Nothing -> (Left . show . head) h -- TODO: make error returning prettier
    where
        h = heap processor
        s = stack processor

state :: forall dom. (HiddenClockResetEnable dom)
  => Signal dom (Either String (Mode, Processor)) 
state = register (Right initialstate) ((>>= step) <$> state)

output :: forall dom. (HiddenClockResetEnable dom)
  => Signal dom (Either String Node)
output = fmap getoutput state 

step :: (Mode, Processor) -> Either String (Mode, Processor)
step (Exit, p) = Right (Exit, p)
step (I, p@(Processor{code=code, pc=pc})) = execInst i p where i = code!!pc
--step (C, p) = execCall 
--step (R, p@Processor{stack=stack, queue=queue}) = execRet n p where n = 
step _ = Left "Unimplemented mode"

execInst :: Inst -> Processor -> Either String (Mode, Processor)
execInst (Inst Store bm args) p@(Processor{pc=progcount, heap=h, queue=q}) = case nodetoheap p bm args h of
    Just (addr, h') -> Right (I, 
        p{heap = h', queue = reftoqueue addr q, pc=progcount+1})
    Nothing -> Left "Error pushing to Heap, cannot Store"

execInst (Inst PushCAF bm args) p@(Processor{pc = progcount, queue = q}) = Right (I, 
    p{queue = primtoqueue (resize $ joinbv args) q, pc = progcount+1})

execInst (Inst PrimOP bm args) p@(Processor{pc = progcount, queue=q}) = Right (I, 
    p{pc = progcount+1, queue = primtoqueue result q}) where
        result = op (tail bigargs) where
            op = getop $ head bigargs
            bigargs = resize <$> args

execInst (Inst Constant bm args) p@(Processor{pc = progcount, queue=q}) = Right (I,
    p{pc = progcount + 1, queue = primtoqueue (resize $ joinbv args) q})

-- execInst (Inst Call bm args) p = Right (C p{})

execInst (Inst Return bm args) p@(Processor{stack=s}) = Right (E, -- SKIP R mode to pass node val
    p{s = push newnode s'} where
        (sframe, s') = pop s
        newnode = StackFrame (push (newS d256 (BitVector (8 * 64)))
)

execInst i _ = Left $ "Missing implementation for instruction: " Data.List.++ (show $ opcode i)

execCall :: Call -> Processor -> Either String (Mode, Processor)
execCall c p = Left $ "Missing implementation for call: " Data.List.++ (show $ c)

execRet :: Node -> Processor -> Either String (Mode, Processor)
execRet = 
