{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}

module Stack where

import Clash.Prelude hiding (Word)

(<~) :: (KnownNat n, Enum i) => Vec n a -> (i, a) -> Vec n a
xs <~ (i, x) = replace i x xs

data Stack n a = Stack {nodesS :: Vec n a, headS :: Index n}

pushS :: (KnownNat n) => a -> Stack n a -> Maybe (Stack n a)
pushS x (Stack vec h) | h == (toEnum $ length vec) = Nothing
                      | otherwise = Just (Stack (vec <~ (h', x)) h')
                      where h' = h + 1

peekS :: (KnownNat n) => Stack n a -> Maybe a
peekS (Stack vec h) | h == 0 = Nothing
                    | otherwise = Just(vec !! h)

popS :: (KnownNat n) => Stack n a -> Maybe (a, Stack n a)
popS (Stack vec h) | h == 0 = Nothing
                   | otherwise = Just (vec!!h, Stack vec (h-1)) -- don't have to remove value, just ignore it


data Queue n a = Queue {nodesQ :: Vec n a, headQ :: Index n, sizeQ :: Index n}

pushQ :: (KnownNat n) => a -> Queue n a -> Queue n a
pushQ x (Queue vec h s) | l == s = Queue (vec <~ (h', x)) h' s
                        | otherwise = Queue (vec <~ (h',x)) h' (s+1)
                        where l = toEnum $ length vec
                              h' = mod (h+1) l

popQ :: (KnownNat n) => Queue n a -> Maybe (a, Queue n a)
popQ (Queue vec h s) | s == 0 = Nothing
                     | otherwise = Just( vec !! h , Queue (vec) h' (s - 1))
                     where l = toEnum $ length vec
                           h' = mod (h-1) l

peekQ :: (KnownNat n) => Index n -> Queue n a -> Maybe a
peekQ i (Queue vec h s) | i >= s = Nothing
                        | otherwise = Just (vec !! i)

class (KnownNat n) => Buffer b (n::Nat) a where
  push :: a -> b n a -> Maybe (b n a)
  safepush :: a -> b n a -> b n a
  safepush x b = case push x b of
    Nothing -> b
    Just(b') -> b'

  pop :: b n a -> Maybe (a, b n a)

instance (KnownNat n) => Buffer Stack n a where
    push x s = pushS x s
    pop s = popS s

instance (KnownNat n) => Buffer Queue n a where
    push x q = Just(pushQ x q)
    pop q = popQ q