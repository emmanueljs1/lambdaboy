module Registers
  ( RegType (..)
  , RegsCompatible (..)
  , CombinedRegs
  , Reg (..)
  , Registers
  , emptyRegisters
  , setReg8
  , reg8
  , setReg16
  , reg16
  )
where

import Data.Bits
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

setReg8 :: Reg r -> Word8 -> Registers -> Registers
setReg8 RegA n8 rs = rs { regA = n8 }
setReg8 RegB n8 rs = rs { regB = n8 }
setReg8 RegC n8 rs = rs { regC = n8 }
setReg8 RegD n8 rs = rs { regD = n8 }
setReg8 RegE n8 rs = rs { regE = n8 }
setReg8 RegH n8 rs = rs { regH = n8 }
setReg8 RegL n8 rs = rs { regL = n8 }

reg8 :: Reg r -> Registers -> Word8
reg8 RegA = regA
reg8 RegB = regB
reg8 RegC = regC
reg8 RegD = regD
reg8 RegE = regE
reg8 RegH = regH
reg8 RegL = regL

setReg16 :: CombinedRegs r1 r2 ~ 'RegsCompatible => Reg r1 -> Reg r2 -> Word16 -> Registers -> Registers
setReg16 reg1 reg2 n16 rs =
  let n8lo = fromIntegral $ shiftR (0xFF00 .&. n16) 8 in
  let n8hi = fromIntegral $ 0x00FF .&. n16 in
  setReg8 reg2 n8hi $ setReg8 reg1 n8lo rs

reg16 :: CombinedRegs r1 r2 ~ 'RegsCompatible => Reg r1 -> Reg r2 -> Registers -> Word16
reg16 reg1 reg2 regs =
  let w1 = shiftL (fromIntegral $ reg8 reg1 regs) 8 in
  let w2 = fromIntegral $ reg8 reg2 regs in
  w1 .|. w2
