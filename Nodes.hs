module Nodes where

import Clash.Prelude hiding (Word)

type Word = (BitVector 64)

data NodeType = CCon | Fun | PFun deriving (Eq, Show, Generic, NFDataX)
data Tag = Tag {nodetype :: NodeType, pcount :: (BitVector 4), mask :: (BitVector 7)} deriving (Eq, Show, Generic, NFDataX)

data Node = Node {tag :: Tag, args :: (Vec 7 Word)} deriving (Eq, Show, Generic, NFDataX)

decodetag :: Word -> Tag
decodetag bits = Tag nodetype pcount mask where
    nodetype = case (firstbits d2 bits) of
        0b00 -> Fun
        0b01 -> PFun
        _ -> CCon
    pcount = 0b0
    mask = lastbits d7 bits

encodetag :: Tag -> Word
encodetag (Tag nodetype pcount mask) =
    packedtype ++# pcount ++# buffer ++# mask where 
        buffer = 0b0 :: BitVector(64 - 2 - 7 - 4)
        packedtype = case nodetype of 
            Fun  -> 0b00
            PFun -> 0b01
            CCon -> 0b10

encodenode :: Node -> BitVector 512
encodenode (Node tag args) = (encodetag tag) ++# joinbv args

decodenode :: BitVector 512 -> Node
decodenode bits = Node (decodetag $ resize bits) (splitbv d64 (lastbits d448 bits))

splitbv :: (KnownNat n, KnownNat m) => SNat m -> BitVector (n*m) -> Vec n (BitVector m)
splitbv a = map v2bv . unconcat a . bv2v

joinbv :: (KnownNat n, KnownNat m) => Vec n (BitVector m) -> BitVector (n*m)
joinbv = v2bv . concat . map bv2v

firstbits :: (KnownNat n, KnownNat m) => SNat n -> BitVector (n + m) -> BitVector n
firstbits n = fst . split
lastbits :: (KnownNat n, KnownNat m) => SNat m -> BitVector (n + m) -> BitVector m
lastbits m = snd . split
slicebits :: (KnownNat a, KnownNat b, KnownNat n, Num (SNat a), (a + b) ~ a) =>
     SNat a -> SNat a -> BitVector (n + a) -> BitVector a
slicebits x y  = firstbits y . lastbits (y - x)

argstonode :: Vec 12 (BitVector 8) -> Node
argstonode bv = Node (Tag nodetype pcount mask) args where
    nodetype = case (firstbits d2 $ head bv) of 
        0b00 -> Fun
        0b01 -> PFun
        _    -> CCon
    pcount = resize (bv !! 1)
    mask = resize (bv !! 2)
    args = (take d7 . drop d3) (map resize bv)