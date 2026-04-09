module Instructions where

import Clash.Prelude hiding (Word)

import Nodes

type Byte = BitVector 8

data Opcode = Store 
            | PushCAF
            | PrimOP
            | Constant
            | Call
            | Force
            | Return
            | Jump
            | Case
            | If
            | Throw
            deriving (Generic, BitPack, Eq, Show, Enum)

encodeopcode :: Opcode -> BitVector 4
encodeopcode = pack

decodeopcode :: BitVector 4 -> Opcode
decodeopcode = unpack

data Inst = Inst {opcode :: Opcode, bitmask :: BitVector 12, args :: Vec 12 Byte} deriving (Eq, Show)

encodeinst :: Inst -> BitVector 128
encodeinst (Inst opcode bitmask args) = resize (encodeopcode opcode) ++# bitmask ++# (joinbv args)
-- decodeinst :: BitVector 128 -> Inst

getop :: (KnownNat n) => Byte -> Vec (n+1) (BitVector 32) -> BitVector 32
getop (0b0) = foldr (+) 0b0
getop (0b1) = foldr1 (-)
getop (0b10) = foldr (*) 0b1
getop _ = const (0b0 :: BitVector 32)