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

type Heap = Vec (2^32) (BitVector (4 * 32)) -- TODO: make into RAM

type RefQueue = Queue 16 (BitVector 32)
type PrimQueue = Queue 16 (BitVector 32)

data MainStack = MainStack {
    nodestack :: NodeStack,
    contstack :: ContStack
}

data MainQueue = MainQueue {
    refqueue  :: RefQueue,
    primqueue :: PrimQueue
}


data Processor = Processor {
    code :: CodeMemory,
    pc   :: PC,

    heap  :: Heap,
    stack :: MainStack,
    queue :: MainQueue
}

data Mode = I | C | E | W | Exit deriving (Eq, Show)

pushref :: MainStack -> Heap -> (Vec 12 Byte) -> MainStack
pushref (MainStack ns cs) h args = undefined

reftoheap :: Word -> Heap -> Maybe (BitVector 32, Heap)
reftoheap r h = case findIndex (== 0b0) h of
    Just empty -> Just (pack empty, h <~ (empty, resize r))
    Nothing -> Nothing

reftoqueue :: (BitVector 32) -> MainQueue -> MainQueue
reftoqueue r q@(MainQueue{refqueue=rq}) = q{refqueue=safepush r rq}
primtoqueue :: (BitVector 32) -> MainQueue -> MainQueue
primtoqueue p q@(MainQueue{primqueue=pq}) = q{primqueue=safepush p pq}

initialstate :: (Mode, Processor)
initialstate = undefined

