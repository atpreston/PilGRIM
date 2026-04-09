module Continuations where

import Clash.Prelude hiding (Word)

import Nodes
import Instructions
import Processor

data CallType = Eval | EvalTLF | TLF | Fix
data Call = Call CallType (Vec 7 Byte)

data ContType = Null | Apply | Select | Catch
data Cont = Cont ContType (Vec 3 Byte)

