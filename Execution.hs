module Execution where

import Clash.Prelude hiding (Word)
import Processor
import Instructions
import Nodes
import Stack
import Continuations

getoutput :: (Mode, Processor) -> Either Node (Vec 4 Word) -- Return node or thrown error
getoutput (Exit, processor) = case peekS s of
    Just nodebits -> Left $ decodenode nodebits
    Nothing -> Right $ splitbv d64 $ resize (h!!0)
    where
        h = heap processor
        s = nodestack $ stack $ processor

-- state :: Signal (Mode, Processor)
-- state = register initialstate (fmap step state)

-- output :: Signal (Either Node (Vec 4 Word))
-- output = fmap getoutput state

step :: (Mode, Processor) -> Either String (Mode, Processor)
step (Exit, p) = Right (Exit, p)
step (I, p@(Processor{code=code, pc=pc})) = execInst i p where i = code!!pc

execInst :: Inst -> Processor -> Either String (Mode, Processor)
execInst (Inst Store bm args) p@(Processor{pc=progcount, heap=h, queue=q}) = case reftoheap (argstonode) h of
    Just (addr, h') -> Right (I, 
        p{heap=h', queue= reftoqueue addr q, pc=progcount+1})
    Nothing -> Left "Heap full, cannot Store"
    where argstonode = encodenode (Node (Tag ))

execInst (Inst PushCAF bm args) p@(Processor{pc=progcount, queue=q}) = Right (I, 
    p{queue= primtoqueue (resize $ joinbv args) q, pc=progcount+1})

execInst (Inst PrimOP bm args) p@(Processor{pc = progcount, queue=q}) = Right (I, 
    p{pc = progcount+1, queue = primtoqueue result q}) where
        result = (getop $ head args) (fmap resize (tail args))

execInst (Inst Constant bm args) p@(Processor{pc = progcount, queue=q}) = Right (I,
    p{pc = progcount + 1, queue = primtoqueue (resize $ joinbv args) q})

-- execInst (Inst Call bm args) p = Right (C p{})
-- execInst _ _ = undefined

execCont :: Cont -> Processor -> Either String (Mode, Processor)
execCont = undefined