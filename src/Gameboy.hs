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

data OffsetType = RegCOffset | Uimm8Offset

data PostInstructionOperationKind = KIncrementAfter | KDecrementAfter

data StackPointerOperationKind = KUnchanged | KAddInt8

data OperandKind
  = KUimm8
  | KImm8
  | KReg8 RegType
  | KUimm16
  | KReg16 RegType RegType
  | KIndirect OperandKind
  | KFF00Offset OffsetType
  | KStackPointer StackPointerOperationKind
  | KPostInstruction OperandKind PostInstructionOperationKind

data Addressable = Addresable | NotAddressable

type family Address (ok :: OperandKind) :: Addressable where
  Address 'KUimm16 = 'Addresable
  Address ('KReg16 _ _) = 'Addresable
  Address _ = 'NotAddressable

data Offsetable = Offsetable OffsetType | NotOffsetable

type family Offset (ok :: OperandKind) :: Offsetable where
  Offset 'KUimm8 = 'Offsetable 'Uimm8Offset
  Offset ('KReg8 'C) = 'Offsetable 'RegCOffset
  Offset _ = 'NotOffsetable

data StackPointerOperation :: StackPointerOperationKind -> * where
  Unchanged :: StackPointerOperation 'KUnchanged
  AddInt8 :: Int8 -> StackPointerOperation 'KAddInt8

deriving instance Show (StackPointerOperation spo)

data PostInstructionOperation :: PostInstructionOperationKind -> * where
  IncrementAfter :: PostInstructionOperation 'KIncrementAfter
  DecrementAfter :: PostInstructionOperation 'KDecrementAfter

deriving instance Show (PostInstructionOperation piok)

data PostOperable = PostOperable | NotPostOperable

type family PostOperation (ok :: OperandKind) (piok :: PostInstructionOperationKind) :: PostOperable where
  PostOperation ('KReg16 'H 'L) 'KIncrementAfter = 'PostOperable
  PostOperation ('KReg16 'H 'L) 'KDecrementAfter = 'PostOperable
  PostOperation _ _ = 'NotPostOperable

data Operand :: OperandKind -> * where
  Uimm8 :: Word8 -> Operand 'KUimm8
  Imm8 :: Int8 -> Operand 'KImm8
  Reg8 :: Reg r -> Operand ('KReg8 r)
  Uimm16 :: Word16 -> Operand 'KUimm16
  Reg16 :: CombinedRegs r1 r2 ~ 'CombinedRegsValid => Reg r1 -> Reg r2 -> Operand ('KReg16 r1 r2)
  Indirect :: Address ok ~ 'Addresable => Operand ok -> Operand ('KIndirect ok)
  FF00Offset :: Offset ok ~ 'Offsetable ot => Operand ok -> Operand ('KFF00Offset ot)
  StackPointer :: StackPointerOperation k -> Operand ('KStackPointer k)
  PostInstruction :: PostOperation ok piok ~ 'PostOperable => Operand ok -> PostInstructionOperation piok -> Operand ('KPostInstruction ok ot)

deriving instance Show (Operand ok)

data Loadable = Loadable | NotLoadable

type family LoadOperands (k1 :: OperandKind) (k2 :: OperandKind) :: Loadable where
  -- LD r8, r8
  LoadOperands ('KReg8 _) ('KReg8 _) = 'Loadable
  -- LD r8, n8
  LoadOperands ('KReg8 _) 'KUimm8 = 'Loadable
  -- LD r16, n16
  LoadOperands ('KReg16 _ _) 'KUimm16 = 'Loadable
  -- LD (HL), r8
  LoadOperands ('KIndirect ('KReg16 'H 'L)) ('KReg8 _) = 'Loadable
  -- LD (HL), n8
  LoadOperands ('KIndirect ('KReg16 'H 'L)) 'KUimm8 = 'Loadable
  -- LD r8, (HL)
  LoadOperands ('KReg8 _) ('KIndirect ('KReg16 'H 'L)) = 'Loadable
  -- LD (r16), A
  LoadOperands ('KIndirect ('KReg16 _ _)) ('KReg8 'A) = 'Loadable
  -- LD (n16), A
  LoadOperands ('KIndirect 'KUimm16) ('KReg8 'A) = 'Loadable
  -- LD ($FF00 + n8), A | LD ($FF00 + C), A
  LoadOperands ('KFF00Offset _) ('KReg8 'A) = 'Loadable
  -- LD A, (r16)
  LoadOperands ('KReg8 'A) ('KIndirect ('KReg16 _ _)) = 'Loadable
  -- LD A, (n16)
  LoadOperands ('KReg8 'A) ('KIndirect 'KUimm16) = 'Loadable
  -- LD A, ($FF00 + n8) | LD A, ($FF00 + C)
  LoadOperands ('KReg8 'A) ('KFF00Offset _) = 'Loadable
  -- LD (HLI), A | LD (HLD), A
  LoadOperands ('KIndirect ('KPostInstruction ('KReg16 'H 'L) _)) ('KReg8 'A) = 'Loadable
  -- LD A, (HLI) | LD A, (HLD)
  LoadOperands ('KReg8 'A) ('KIndirect ('KPostInstruction ('KReg16 'H 'L) _)) = 'Loadable
  -- LD SP, n16
  LoadOperands ('KStackPointer 'KUnchanged) 'KUimm16 = 'Loadable
  -- LD (n16), SP
  LoadOperands ('KIndirect 'KUimm16) ('KStackPointer 'KUnchanged) = 'Loadable
  -- LD HL, SP + e8
  LoadOperands ('KReg16 'H 'L) ('KStackPointer 'KAddInt8) = 'Loadable
  -- LD SP, HL
  LoadOperands ('KStackPointer 'KUnchanged) ('KReg16 'H 'L) = 'Loadable
  LoadOperands _ _ = 'NotLoadable

data AddTypeKind = KWithCarryAdded | KWithoutCarryAdded

data AddType :: AddTypeKind -> * where
  WithCarryAdded :: AddType 'KWithCarryAdded
  WithoutCarryAdded :: AddType 'KWithoutCarryAdded

data Addable = Addable | NotAddable

type family AddOperands (atk :: AddTypeKind) (k1 :: OperandKind) (k2 :: OperandKind) :: Addable where
  AddOperands _ ('KReg8 'A) ('KReg8 _) = 'Addable
  AddOperands _ ('KReg8 'A) ('KIndirect ('KReg16 'H 'L)) = 'Addable
  AddOperands _ ('KReg8 'A) 'KUimm8 = 'Addable
  AddOperands 'KWithoutCarryAdded ('KReg16 'H 'L) ('KStackPointer 'KUnchanged) = 'Addable
  AddOperands 'KWithoutCarryAdded ('KStackPointer 'KUnchanged) 'KImm8 = 'Addable
  AddOperands _ _ _ = 'NotAddable

data Andable = Andable | NotAndable

type family AndOperands (k1 :: OperandKind) (k2 :: OperandKind) :: Andable where
  AndOperands ('KReg8 'A) ('KReg8 _) = 'Andable
  AndOperands ('KReg8 'A) ('KIndirect ('KReg16 'H 'L)) = 'Andable
  AndOperands ('KReg8 'A) 'KUimm8 = 'Andable
  AndOperands _ _ = 'NotAndable

data Comparable = Comparable | NotComparable

type family CompareOperands (k1 :: OperandKind) (k2 :: OperandKind) :: Comparable where
  CompareOperands ('KReg8 'A) ('KReg8 _) = 'Comparable
  CompareOperands ('KReg8 'A) ('KIndirect ('KReg16 'H 'L)) = 'Comparable
  CompareOperands ('KReg8 'A) 'KUimm8 = 'Comparable
  CompareOperands _ _ = 'NotComparable

data Decrementable = Decrementable | NotDecrementable

type family DecrementOperand (ok :: OperandKind) :: Decrementable where
  DecrementOperand ('KReg8 _) = 'Decrementable
  DecrementOperand ('KIndirect ('KReg16 'H 'L)) = 'Decrementable
  DecrementOperand ('KReg16 _ _) = 'Decrementable
  DecrementOperand ('KStackPointer 'KUnchanged) = 'Decrementable
  DecrementOperand _ = 'NotDecrementable

data Incrementable = Incrementable | NotIncrementable

type family IncrementOperand (ok :: OperandKind) :: Incrementable where
  IncrementOperand ('KReg8 _) = 'Incrementable
  IncrementOperand ('KIndirect ('KReg16 'H 'L)) = 'Incrementable
  IncrementOperand ('KReg16 _ _) = 'Incrementable
  IncrementOperand ('KStackPointer 'KUnchanged) = 'Incrementable
  IncrementOperand _ = 'NotIncrementable

data InstructionKind
  = KLoad
  | KAdd
  | KAnd
  | KCompare
  | KDecrement
  | KIncrement

data Instruction :: InstructionKind -> * where
  Load :: LoadOperands k1 k2 ~ 'Loadable => Operand k1 -> Operand k2 -> Instruction 'KLoad
  Add :: AddOperands atk k1 k2 ~ 'Addable => AddType atk -> Operand k1 -> Operand k2 -> Instruction 'KAdd
  And :: AndOperands k1 k2 ~ 'Andable => Operand k1 -> Operand k2 -> Instruction 'KAnd
  Compare :: CompareOperands k1 k2 ~ 'Comparable => Operand k1 -> Operand k2 -> Instruction 'KCompare
  Decrement :: DecrementOperand ok ~ 'Decrementable => Operand ok -> Instruction 'KDecrement
  Increment :: IncrementOperand ok ~ 'Incrementable => Operand ok -> Instruction 'KIncrement

executeInstruction :: Instruction k -> IO ()
executeInstruction ins@(Load _ _) = loadIns ins where
  loadIns :: Instruction 'KLoad -> IO ()
  loadIns _ = undefined -- TODO: implement
executeInstruction ins@(Add _ _ _) = addIns ins where
  addIns :: Instruction 'KAdd -> IO ()
  addIns (Add WithCarryAdded _ _) = undefined -- TODO: implement
  addIns (Add WithoutCarryAdded _ _) = undefined -- TODO: implement
executeInstruction ins@(And _ _) = andIns ins where
  andIns :: Instruction 'KAnd -> IO ()
  andIns _ = undefined -- TODO: implement
executeInstruction ins@(Compare _ _) = compareIns ins where
  compareIns :: Instruction 'KCompare -> IO ()
  compareIns _ = undefined -- TODO: implement
executeInstruction ins@(Decrement _) = decrementIns ins where
  decrementIns :: Instruction 'KDecrement -> IO ()
  decrementIns = undefined -- TODO: implement
executeInstruction ins@(Increment _) = incrementIns ins where
  incrementIns :: Instruction 'KIncrement -> IO ()
  incrementIns = undefined -- TODO: implement

someFunc :: IO ()
someFunc = do
  let instruction = Load (Reg8 RegA) (Reg8 RegB)
  executeInstruction instruction
