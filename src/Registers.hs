module Registers
  ( RegType (..)
  , RegsCompatible (..)
  , CombinedRegs
  , Reg (..)
  )
where

import Data.Kind

data RegType
  = A
  | B
  | C
  | D
  | E
  | H
  | L

data RegsCompatible = RegsCompatible | RegsNotCompatible

type family CombinedRegs (r1 :: RegType) (r2 :: RegType) :: RegsCompatible where
  CombinedRegs 'B 'C = 'RegsCompatible
  CombinedRegs 'D 'E = 'RegsCompatible
  CombinedRegs 'H 'L = 'RegsCompatible
  CombinedRegs _ _ = 'RegsNotCompatible

data Reg :: RegType -> Type where
  RegA :: Reg 'A
  RegB :: Reg 'B
  RegC :: Reg 'C
  RegD :: Reg 'D
  RegE :: Reg 'E
  RegH :: Reg 'H
  RegL :: Reg 'L

deriving instance Show (Reg rt)
