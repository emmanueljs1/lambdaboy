{-# LANGUAGE GADTs, DataKinds, TypeFamilies, StandaloneDeriving #-}
module Gameboy
    ( someFunc
    ) where

import Data.Int
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

data OffsetType = RegCOffset | Imm8Offset

data PostOperationType = Increment | Decrement

data StackPointerOperation = None | AddInt8

data OperandKind
  = KImm8
  | KReg8 RegType
  | KImm16
  | KReg16 RegType RegType
  | KIndirect OperandKind
  | KFF00Offset OffsetType
  | KStackPointer StackPointerOperation
  | KPostInstruction OperandKind PostOperationType

data Addressable = Addresable | NotAddressable

type family Address (ok :: OperandKind) :: Addressable where
  Address 'KImm16 = 'Addresable
  Address ('KReg16 _ _) = 'Addresable
  Address _ = 'NotAddressable

data Offsetable = Offsetable OffsetType | NotOffsetable

type family Offset (ok :: OperandKind) :: Offsetable where
  Offset 'KImm8 = 'Offsetable 'Imm8Offset
  Offset ('KReg8 'C) = 'Offsetable 'RegCOffset
  Offset _ = 'NotOffsetable

data StackPointerType :: StackPointerOperation -> * where
  Default :: StackPointerType 'None
  AddI8 :: Int8 -> StackPointerType 'AddInt8

deriving instance Show (StackPointerType spo)

data PostInstructionOp :: PostOperationType -> * where
  Incr :: PostInstructionOp 'Increment
  Decr :: PostInstructionOp 'Decrement

deriving instance Show (PostInstructionOp ot)

data PostOperable = PostOperable | NotPostOperable

type family PostOperation (ok :: OperandKind) (ot :: PostOperationType) :: PostOperable where
  PostOperation ('KReg16 'H 'L) 'Increment = 'PostOperable
  PostOperation ('KReg16 'H 'L) 'Decrement = 'PostOperable
  PostOperation _ _ = 'NotPostOperable

data Operand :: OperandKind -> * where
  Imm8 :: Word8 -> Operand 'KImm8
  Reg8 :: Reg r -> Operand ('KReg8 r)
  Imm16 :: Word16 -> Operand 'KImm16
  Reg16 :: CombinedRegs r1 r2 ~ 'CombinedRegsValid => Reg r1 -> Reg r2 -> Operand ('KReg16 r1 r2)
  Indirect :: Address ok ~ 'Addresable => Operand ok -> Operand ('KIndirect ok)
  FF00Offset :: Offset ok ~ 'Offsetable ot => Operand ok -> Operand ('KFF00Offset ot)
  StackPointer :: StackPointerType spo -> Operand ('KStackPointer spo)
  PostInstruction :: PostOperation ok ot ~ 'PostOperable => Operand ok -> PostInstructionOp ot -> Operand ('KPostInstruction ok ot)

deriving instance Show (Operand ok)

data Loadable = Loadable | NotLoadable

type family LoadOperands (k1 :: OperandKind) (k2 :: OperandKind) :: Loadable where
  -- LD r8, r8
  LoadOperands ('KReg8 _) ('KReg8 _) = 'Loadable
  -- LD r8, n8
  LoadOperands ('KReg8 _) 'KImm8 = 'Loadable
  -- LD r16, n16
  LoadOperands ('KReg16 _ _) 'KImm16 = 'Loadable
  -- LD (HL), r8
  LoadOperands ('KIndirect ('KReg16 'H 'L)) ('KReg8 _) = 'Loadable
  -- LD (HL), n8
  LoadOperands ('KIndirect ('KReg16 'H 'L)) 'KImm8 = 'Loadable
  -- LD r8, (HL)
  LoadOperands ('KReg8 _) ('KIndirect ('KReg16 'H 'L)) = 'Loadable
  -- LD (r16), A
  LoadOperands ('KIndirect ('KReg16 _ _)) ('KReg8 'A) = 'Loadable
  -- LD (n16), A
  LoadOperands ('KIndirect 'KImm16) ('KReg8 'A) = 'Loadable
  -- LD ($FF00 + n8), A
  LoadOperands ('KFF00Offset 'Imm8Offset) ('KReg8 'A) = 'Loadable
  -- LD ($FF00 + C), A
  LoadOperands ('KFF00Offset 'RegCOffset) ('KReg8 'A) = 'Loadable
  -- LD A, (r16)
  LoadOperands ('KReg8 'A) ('KIndirect ('KReg16 _ _)) = 'Loadable
  -- LD A, (n16)
  LoadOperands ('KReg8 'A) ('KIndirect 'KImm16) = 'Loadable
  -- LD A, ($FF00 + n8)
  LoadOperands ('KReg8 'A) ('KFF00Offset 'Imm8Offset) = 'Loadable
  -- LD A, ($FF00 + C)
  LoadOperands ('KReg8 'A) ('KFF00Offset 'RegCOffset) = 'Loadable
  -- LD (HLI), A
  LoadOperands ('KIndirect ('KPostInstruction ('KReg16 'H 'L) 'Increment)) ('KReg8 'A) = 'Loadable
  -- LD (HLD), A
  LoadOperands ('KIndirect ('KPostInstruction ('KReg16 'H 'L) 'Decrement)) ('KReg8 'A) = 'Loadable
  -- LD A, (HLI)
  LoadOperands ('KReg8 'A) ('KIndirect ('KPostInstruction ('KReg16 'H 'L) 'Increment)) = 'Loadable
  -- LD A, (HLD)
  LoadOperands ('KReg8 'A) ('KIndirect ('KPostInstruction ('KReg16 'H 'L) 'Decrement)) = 'Loadable
  -- LD SP, n16
  LoadOperands ('KStackPointer 'None) 'KImm16 = 'Loadable
  -- LD (n16), SP
  LoadOperands ('KIndirect 'KImm16) ('KStackPointer 'None) = 'Loadable
  -- LD HL, SP + e8
  LoadOperands ('KReg16 'H 'L) ('KStackPointer 'AddInt8) = 'Loadable
  -- LD SP, HL
  LoadOperands ('KStackPointer 'None) ('KReg16 'H 'L) = 'Loadable
  LoadOperands _ _ = 'NotLoadable

data Carry = CarryFlag | NoCarry

data Addable = Addable | NotAddable

type family AddOperands (k1 :: OperandKind) (k2 :: OperandKind) :: Addable where
  AddOperands ('KReg8 'A) ('KReg8 _) = 'Addable
  AddOperands ('KReg8 'A) ('KIndirect ('KReg16 'H 'L)) = 'Addable
  AddOperands ('KReg8 'A) 'KImm8 = 'Addable
  AddOperands _ _ = 'NotAddable

data Andable = Andable | NotAndable

type family AndOperands (k1 :: OperandKind) (k2 :: OperandKind) :: Andable where
  AndOperands ('KReg8 'A) ('KReg8 _) = 'Andable
  AndOperands ('KReg8 'A) ('KIndirect ('KReg16 'H 'L)) = 'Andable
  AndOperands ('KReg8 'A) 'KImm8 = 'Andable
  AndOperands _ _ = 'NotAndable

data InstructionKind
  = KLoad
  | KAdd
  | KAnd

data Instruction :: InstructionKind -> * where
  Load :: LoadOperands k1 k2 ~ 'Loadable => Operand k1 -> Operand k2 -> Instruction 'KLoad
  Add :: AddOperands k1 k2 ~ 'Addable => Carry -> Operand k1 -> Operand k2 -> Instruction 'KAdd
  And :: AndOperands k1 k2 ~ 'Andable => Operand k1 -> Operand k2 -> Instruction 'KAnd

executeInstruction :: Instruction k -> IO ()
executeInstruction ins@(Load _ _) = loadIns ins where
  loadIns :: Instruction 'KLoad -> IO ()
  loadIns _ = undefined -- TODO: implement
executeInstruction ins@(Add _ _ _) = addIns ins where
  addIns :: Instruction 'KAdd -> IO ()
  addIns _ = undefined -- TODO: implement
executeInstruction ins@(And _ _) = andIns ins where
  andIns :: Instruction 'KAnd -> IO ()
  andIns _ = undefined -- TODO: implement

someFunc :: IO ()
someFunc = do
  let instruction = Load (Reg8 RegA) (Reg8 RegB)
  executeInstruction instruction
