{-# LANGUAGE GADTs, DataKinds, TypeFamilies, StandaloneDeriving #-}
module Gameboy
    ( someFunc
    ) where

import Data.Word

data RegType
  = A
  | B
  | C
  | D
  | E
  | H
  | L

data CombinedRegValidity = CombinedRegsValid | CombinedRegsInvalid

type family CombinedRegs (r1 :: RegType) (r2 :: RegType) :: CombinedRegValidity where
  CombinedRegs 'B 'C = 'CombinedRegsValid
  CombinedRegs 'D 'E = 'CombinedRegsValid
  CombinedRegs 'H 'L = 'CombinedRegsValid
  CombinedRegs _ _ = 'CombinedRegsInvalid

data Reg :: RegType -> * where
  RegA :: Reg 'A
  RegB :: Reg 'B
  RegC :: Reg 'C
  RegD :: Reg 'D
  RegE :: Reg 'E
  RegH :: Reg 'H
  RegL :: Reg 'L

deriving instance Show (Reg rt)

data OperandKind
  = KImm8
  | KReg8
  | KImm16
  | KReg16
  | KIndirect

data Addressable = Addresable | NotAddressable

type family Address (ok :: OperandKind) :: Addressable where
  Address 'KImm16 = 'Addresable
  Address 'KReg16 = 'Addresable
  Address _ = 'NotAddressable

data Operand :: OperandKind -> * where
  Imm8 :: Word8 -> Operand 'KImm8
  Reg8 :: Reg r -> Operand 'KReg8
  Imm16 :: Word16 -> Operand 'KImm16
  Reg16 :: CombinedRegs r1 r2 ~ 'CombinedRegsValid => Reg r1 -> Reg r2 -> Operand 'KReg16
  Indirect :: Address ok ~ 'Addresable => Operand ok -> Operand 'KIndirect

deriving instance Show (Operand ok)

data Loadable = Loadable | NotLoadable

type family LoadOperands (k1 :: OperandKind) (k2 :: OperandKind) :: Loadable where
  -- TODO: add all valid load operand combinations
  LoadOperands 'KReg8 'KReg8 = 'Loadable
  LoadOperands _ _ = 'NotLoadable

data InstructionKind
  = KLoad

data Instruction :: InstructionKind -> * where
  Load :: LoadOperands k1 k2 ~ 'Loadable => Operand k1 -> Operand k2 -> Instruction 'KLoad

executeInstruction :: Instruction k -> IO ()
executeInstruction ins@(Load _ _) = load ins where
  load :: Instruction 'KLoad -> IO ()
  load _ = undefined -- TODO: implement

someFunc :: IO ()
someFunc = do
  let instruction = Load (Reg8 RegA) (Reg8 RegB)
  executeInstruction instruction
