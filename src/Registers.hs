module Registers
  ( RegType (..)
  , RegsCompatible (..)
  , CombinedRegs
  , Reg (..)
  , Registers
  , emptyRegisters
  , setReg
  , reg
  )
where

import Data.Kind
import Data.Word

data RegType
  = A
  | B
  | C
  | D
  | E
  | H
  | L

data RegsCompatible = RegsCompatible

type family CombinedRegs (r1 :: RegType) (r2 :: RegType) :: RegsCompatible where
  CombinedRegs 'B 'C = 'RegsCompatible
  CombinedRegs 'D 'E = 'RegsCompatible
  CombinedRegs 'H 'L = 'RegsCompatible

data Reg :: RegType -> Type where
  RegA :: Reg 'A
  RegB :: Reg 'B
  RegC :: Reg 'C
  RegD :: Reg 'D
  RegE :: Reg 'E
  RegH :: Reg 'H
  RegL :: Reg 'L

deriving instance Show (Reg rt)

data Registers = Registers {
  regA :: Word8,
  regB :: Word8,
  regC :: Word8,
  regD :: Word8,
  regE :: Word8,
  regH :: Word8,
  regL :: Word8
}

emptyRegisters :: Registers
emptyRegisters = Registers 0 0 0 0 0 0 0

setReg :: Reg r -> Word8 -> Registers -> Registers
setReg RegA word rs = rs { regA = word }
setReg RegB word rs = rs { regB = word }
setReg RegC word rs = rs { regC = word }
setReg RegD word rs = rs { regD = word }
setReg RegE word rs = rs { regE = word }
setReg RegH word rs = rs { regH = word }
setReg RegL word rs = rs { regL = word }

reg :: Reg r -> Registers -> Word8
reg RegA = regA
reg RegB = regB
reg RegC = regC
reg RegD = regD
reg RegE = regE
reg RegH = regH
reg RegL = regL
