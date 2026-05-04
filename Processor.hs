module Processor where

import Clash.Prelude hiding (Word)

import Nodes
import Instructions
import Stack

type CodeMemory = Vec (2^16) Inst
type PC = BitVector 16


type NodeStack = Stack 256 (BitVector (8 * 64)) -- Unstated length
-- TODO: make sure only top 4 entries can be read
type ContStack = Stack 16 (BitVector (2 * 64))-- Unstated length

type Heap = Vec (2^32) (BitVector (4 *  64 * 2)) -- NOTE: twice size due to no variable encoding

type RefQueue = Queue 16 (BitVector 64)
type PrimQueue = Queue 16 (BitVector 64)

data Target = Rnto | Rrto | Rcase | Rnext | Rmain deriving (Generic, NFDataX)

type MainStack = Stack 256 (StackFrame)

data StackFrame = StackFrame {
    nodestack :: NodeStack,
    contstack :: ContStack,
    returntarget :: Target
} deriving (Generic, NFDataX)

data MainQueue = MainQueue {
    refqueue  :: RefQueue,
    primqueue :: PrimQueue
} deriving (Generic, NFDataX)


data Processor = Processor {
    code :: CodeMemory,
    pc   :: PC,

    heap  :: Heap,
    stack :: MainStack,
    queue :: MainQueue
} deriving (Generic, NFDataX)

data Mode = I | C | E | W | Exit deriving (Eq, Show, Generic, NFDataX)

pushtoheap :: Heap -> BitVector (4 *  64 * 2) -> Maybe (Word, Heap)
pushtoheap h x = (\i -> (resize $ pack i, replace i x h)) <$> elemIndex 0 h

deref :: (KnownNat n, KnownNat m) 
  => Queue (n+1) (BitVector m) -> (Bit, Byte) -> Maybe (BitVector m)
deref q (flag, val) = case flag of 
    0b0 -> (Just . resize) val
    0b1 -> peekQ ref q
    where ref = (bitCoerce . resize) val

nodetoheap :: Processor -> BitVector 12 -> Vec 12 Byte -> Heap -> Maybe (Word, Heap)
nodetoheap p bv args heap = node >>= (pushtoheap heap) where
    node = (encodenode . (Node tag)) <$> (take d7 <$> vals)
    vals = traverse (deref (primqueue $ queue $ p)) (tail $ zip (bv2v bv) args)
    tag = decodetag $ resize (head args)



reftoqueue :: (BitVector 64) -> MainQueue -> MainQueue
reftoqueue r q@(MainQueue{refqueue=rq}) = q{refqueue=safepush r rq}
primtoqueue :: (BitVector 64) -> MainQueue -> MainQueue
primtoqueue p q@(MainQueue{primqueue=pq}) = q{primqueue=safepush p pq}


-- nodetostack :: (BitVector 64) -> MainStack -> MainStack
-- nodetostack n stack @(MainStack{nodestack=ns}) = s{nodestack=safepush (resize n) ns}
-- conttostack :: (BitVector 64) -> MainStack -> MainStack
-- conttostack c s@(MainStack{contstack=cs}) = s{contstack=safepush (resize c) cs}

initialstate :: (Mode, Processor)
initialstate = undefined