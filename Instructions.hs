module Instructions where

import Clash.Prelude hiding (Word)

import Nodes

type Byte = BitVector 8

data Opcode = Store 
            | PushCAF
            | PrimOP
            | Constant
            | CallInst
            | Force
            | Return
            | Jump
            | Case
            | If
            | Throw
            deriving (Generic, BitPack, Eq, Show, Enum, NFDataX)

encodeopcode :: Opcode -> BitVector 4
encodeopcode = pack

decodeopcode :: BitVector 4 -> Opcode
decodeopcode = unpack

data Inst = Inst {opcode :: Opcode, bitmask :: BitVector 12, args :: Vec 12 Byte} deriving (Eq, Show, Generic, NFDataX)

encodeinst :: Inst -> BitVector 128
encodeinst (Inst opcode bitmask args) = resize (encodeopcode opcode) ++# bitmask ++# (joinbv args)
-- decodeinst :: BitVector 128 -> Inst

getop :: (KnownNat n) => Word -> Vec (n+1) (Word) -> Word
getop (0b0) = foldr1 (+)
getop (0b1) = foldr1 (-)
getop (0b10) = foldr1 (*)
getop (0b11) = foldr1 (div)
getop (0b100) = (\xs -> (foldr (+) 0 (tail xs)) `mod` (head xs))
getop _ = const 0b0