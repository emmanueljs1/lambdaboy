{-# LANGUAGE GADTs, DataKinds, TypeFamilies, StandaloneDeriving, ConstraintKinds, TypeOperators #-}


module Gameboy
    ( someFunc
    ) where

import Data.Int
import Data.Word
import GHC.TypeLits

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
  | KUimm3

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

data Uimm3Nat (n :: Nat) = Uimm3Nat

instance KnownNat n => Show (Uimm3Nat n) where
  show b = "BitToTest " ++ show (natVal b)

type Only3Bits (n :: Nat) = (KnownNat n, 0 <= n, n <= 7)

data Operand :: OperandKind -> * where
  Uimm8 :: Word8 -> Operand 'KUimm8
  Imm8 :: Int8 -> Operand 'KImm8
  Reg8 :: Reg r -> Operand ('KReg8 r)
  Uimm16 :: Word16 -> Operand 'KUimm16
  Reg16 :: CombinedRegs r1 r2 ~ 'RegsCompatible => Reg r1 -> Reg r2 -> Operand ('KReg16 r1 r2)
  Indirect :: Address ok ~ 'Addresable => Operand ok -> Operand ('KIndirect ok)
  FF00Offset :: Offset ok ~ 'Offsetable ot => Operand ok -> Operand ('KFF00Offset ot)
  StackPointer :: StackPointerOperation k -> Operand ('KStackPointer k)
  PostInstruction :: PostOperation ok piok ~ 'PostOperable => Operand ok -> PostInstructionOperation piok -> Operand ('KPostInstruction ok piok)
  Uimm3 :: Only3Bits n => Uimm3Nat n -> Operand 'KUimm3

deriving instance Show (Operand ok)

data InstructionKind
  = KLoad
  | KAdd
  | KAnd
  | KCompare
  | KDecrement
  | KIncrement
  | KOr
  | KSub
  | KXor
  | KBit
  | KRes
  | KSet
  | KSwap
  | KInvalid

type family LoadOperands (k1 :: OperandKind) (k2 :: OperandKind) :: InstructionKind where
  -- LD r8, r8
  LoadOperands ('KReg8 _) ('KReg8 _) = 'KLoad
  -- LD r8, n8
  LoadOperands ('KReg8 _) 'KUimm8 = 'KLoad
  -- LD r16, n16
  LoadOperands ('KReg16 _ _) 'KUimm16 = 'KLoad
  -- LD (HL), r8
  LoadOperands ('KIndirect ('KReg16 'H 'L)) ('KReg8 _) = 'KLoad
  -- LD (HL), n8
  LoadOperands ('KIndirect ('KReg16 'H 'L)) 'KUimm8 = 'KLoad
  -- LD r8, (HL)
  LoadOperands ('KReg8 _) ('KIndirect ('KReg16 'H 'L)) = 'KLoad
  -- LD (r16), A
  LoadOperands ('KIndirect ('KReg16 _ _)) ('KReg8 'A) = 'KLoad
  -- LD (n16), A
  LoadOperands ('KIndirect 'KUimm16) ('KReg8 'A) = 'KLoad
  -- LD ($FF00 + n8), A | LD ($FF00 + C), A
  LoadOperands ('KFF00Offset _) ('KReg8 'A) = 'KLoad
  -- LD A, (r16)
  LoadOperands ('KReg8 'A) ('KIndirect ('KReg16 _ _)) = 'KLoad
  -- LD A, (n16)
  LoadOperands ('KReg8 'A) ('KIndirect 'KUimm16) = 'KLoad
  -- LD A, ($FF00 + n8) | LD A, ($FF00 + C)
  LoadOperands ('KReg8 'A) ('KFF00Offset _) = 'KLoad
  -- LD (HLI), A | LD (HLD), A
  LoadOperands ('KIndirect ('KPostInstruction ('KReg16 'H 'L) _)) ('KReg8 'A) = 'KLoad
  -- LD A, (HLI) | LD A, (HLD)
  LoadOperands ('KReg8 'A) ('KIndirect ('KPostInstruction ('KReg16 'H 'L) _)) = 'KLoad
  -- LD SP, n16
  LoadOperands ('KStackPointer 'KUnchanged) 'KUimm16 = 'KLoad
  -- LD (n16), SP
  LoadOperands ('KIndirect 'KUimm16) ('KStackPointer 'KUnchanged) = 'KLoad
  -- LD HL, SP + e8
  LoadOperands ('KReg16 'H 'L) ('KStackPointer 'KAddInt8) = 'KLoad
  -- LD SP, HL
  LoadOperands ('KStackPointer 'KUnchanged) ('KReg16 'H 'L) = 'KLoad
  LoadOperands _ _ = 'KInvalid

data ArithmeticTypeKind = KWithCarryIncluded | KWithoutCarryIncluded

data ArithmeticType :: ArithmeticTypeKind -> * where
  WithCarryIncluded :: ArithmeticType 'KWithCarryIncluded
  WithoutCarryIncluded :: ArithmeticType 'KWithoutCarryIncluded

type family AddOperands (atk :: ArithmeticTypeKind) (k1 :: OperandKind) (k2 :: OperandKind) :: InstructionKind where
  AddOperands _ ('KReg8 'A) ('KReg8 _) = 'KAdd
  AddOperands _ ('KReg8 'A) ('KIndirect ('KReg16 'H 'L)) = 'KAdd
  AddOperands _ ('KReg8 'A) 'KUimm8 = 'KAdd
  AddOperands 'KWithoutCarryIncluded ('KReg16 'H 'L) ('KStackPointer 'KUnchanged) = 'KAdd
  AddOperands 'KWithoutCarryIncluded ('KStackPointer 'KUnchanged) 'KImm8 = 'KAdd
  AddOperands _ _ _ = 'KInvalid

type family AndOperands (k1 :: OperandKind) (k2 :: OperandKind) :: InstructionKind where
  AndOperands ('KReg8 'A) ('KReg8 _) = 'KAnd
  AndOperands ('KReg8 'A) ('KIndirect ('KReg16 'H 'L)) = 'KAnd
  AndOperands ('KReg8 'A) 'KUimm8 = 'KAnd
  AndOperands _ _ = 'KInvalid

type family CompareOperands (k1 :: OperandKind) (k2 :: OperandKind) :: InstructionKind where
  CompareOperands ('KReg8 'A) ('KReg8 _) = 'KCompare
  CompareOperands ('KReg8 'A) ('KIndirect ('KReg16 'H 'L)) = 'KCompare
  CompareOperands ('KReg8 'A) 'KUimm8 = 'KCompare
  CompareOperands _ _ = 'KInvalid

type family DecrementOperand (ok :: OperandKind) :: InstructionKind where
  DecrementOperand ('KReg8 _) = 'KDecrement
  DecrementOperand ('KIndirect ('KReg16 'H 'L)) = 'KDecrement
  DecrementOperand ('KReg16 _ _) = 'KDecrement
  DecrementOperand ('KStackPointer 'KUnchanged) = 'KDecrement
  DecrementOperand _ = 'KInvalid

type family IncrementOperand (ok :: OperandKind) :: InstructionKind where
  IncrementOperand ('KReg8 _) = 'KIncrement
  IncrementOperand ('KIndirect ('KReg16 'H 'L)) = 'KIncrement
  IncrementOperand ('KReg16 _ _) = 'KIncrement
  IncrementOperand ('KStackPointer 'KUnchanged) = 'KIncrement
  IncrementOperand _ = 'KInvalid

type family OrOperands (k1 :: OperandKind) (k2 :: OperandKind) :: InstructionKind where
  OrOperands ('KReg8 'A) ('KReg8 _) = 'KOr
  OrOperands ('KReg8 'A) ('KIndirect ('KReg16 'H 'L)) = 'KOr
  OrOperands ('KReg8 'A) 'KUimm8 = 'KOr
  OrOperands _ _ = 'KInvalid

type family SubOperands (k1 :: OperandKind) (k2 :: OperandKind) :: InstructionKind where
  SubOperands ('KReg8 'A) ('KReg8 _) = 'KSub
  SubOperands ('KReg8 'A) ('KIndirect ('KReg16 'H 'L)) = 'KSub
  SubOperands ('KReg8 'A) 'KUimm8 = 'KSub
  SubOperands _ _ = 'KInvalid

type family XorOperands (k1 :: OperandKind) (k2 :: OperandKind) :: InstructionKind where
  XorOperands ('KReg8 'A) ('KReg8 _) = 'KXor
  XorOperands ('KReg8 'A) ('KIndirect ('KReg16 'H 'L)) = 'KXor
  XorOperands ('KReg8 'A) 'KUimm8 = 'KXor
  XorOperands _ _ = 'KInvalid

type family BitOperands (k1 :: OperandKind) (k2 :: OperandKind) :: InstructionKind where
  BitOperands 'KUimm3 ('KReg8 _) = 'KBit
  BitOperands 'KUimm3 ('KIndirect ('KReg16 'H 'L)) = 'KBit
  BitOperands _ _ = 'KInvalid

type family ResOperands (k1 :: OperandKind) (k2 :: OperandKind) :: InstructionKind where
  ResOperands 'KUimm3 ('KReg8 _) = 'KRes
  ResOperands 'KUimm3 ('KIndirect ('KReg16 'H 'L)) = 'KRes
  ResOperands _ _ = 'KInvalid

type family SetOperands (k1 :: OperandKind) (k2 :: OperandKind) :: InstructionKind where
  SetOperands 'KUimm3 ('KReg8 _) = 'KSet
  SetOperands 'KUimm3 ('KIndirect ('KReg16 'H 'L)) = 'KSet
  SetOperands _ _ = 'KInvalid

type family SwapOperand (ok :: OperandKind) :: InstructionKind where
  SwapOperand ('KReg8 _) = 'KSwap
  SwapOperand ('KIndirect ('KReg16 'H 'L)) = 'KSwap
  SwapOperand _ = 'KInvalid

data Instruction :: InstructionKind -> * where
  Load :: LoadOperands k1 k2 ~ 'KLoad => Operand k1 -> Operand k2 -> Instruction 'KLoad
  Add :: AddOperands atk k1 k2 ~ 'KAdd => ArithmeticType atk -> Operand k1 -> Operand k2 -> Instruction 'KAdd
  And :: AndOperands k1 k2 ~ 'KAnd => Operand k1 -> Operand k2 -> Instruction 'KAnd
  Compare :: CompareOperands k1 k2 ~ 'KCompare => Operand k1 -> Operand k2 -> Instruction 'KCompare
  Decrement :: DecrementOperand ok ~ 'KDecrement => Operand ok -> Instruction 'KDecrement
  Increment :: IncrementOperand ok ~ 'KIncrement => Operand ok -> Instruction 'KIncrement
  Or :: OrOperands k1 k2 ~ 'KOr => Operand k1 -> Operand k2 -> Instruction 'KOr
  Sub :: SubOperands k1 k2 ~ 'KSub => ArithmeticType atk -> Operand k1 -> Operand k2 -> Instruction 'KSub
  Xor :: XorOperands k1 k2 ~ 'KXor => Operand k1 -> Operand k2 -> Instruction 'KXor
  Bit :: BitOperands k1 k2 ~ 'KBit => Operand k1 -> Operand k2 -> Instruction 'KBit
  Res :: ResOperands k1 k2 ~ 'KRes => Operand k1 -> Operand k2 -> Instruction 'KRes
  Set :: SetOperands k1 k2 ~ 'KSet => Operand k1 -> Operand k2 -> Instruction 'KSet
  Swap :: SwapOperand ok ~ 'KSwap => Operand ok -> Instruction 'KSwap

-- TODO: implement
executeInstruction :: Instruction k -> IO ()
executeInstruction ins@(Load _ _) = loadIns ins where
  loadIns :: Instruction 'KLoad -> IO ()
  loadIns _ = undefined
executeInstruction ins@(Add _ _ _) = addIns ins where
  addIns :: Instruction 'KAdd -> IO ()
  addIns (Add WithCarryIncluded _ _) = undefined
  addIns (Add WithoutCarryIncluded _ _) = undefined
executeInstruction ins@(And _ _) = andIns ins where
  andIns :: Instruction 'KAnd -> IO ()
  andIns _ = undefined
executeInstruction ins@(Compare _ _) = compareIns ins where
  compareIns :: Instruction 'KCompare -> IO ()
  compareIns _ = undefined
executeInstruction ins@(Decrement _) = decrementIns ins where
  decrementIns :: Instruction 'KDecrement -> IO ()
  decrementIns = undefined
executeInstruction ins@(Increment _) = incrementIns ins where
  incrementIns :: Instruction 'KIncrement -> IO ()
  incrementIns _ = undefined
executeInstruction ins@(Or _ _) = orIns ins where
  orIns :: Instruction 'KOr -> IO ()
  orIns = undefined
executeInstruction ins@(Sub _ _ _) = subIns ins where
  subIns :: Instruction 'KSub -> IO ()
  subIns = undefined
executeInstruction ins@(Xor _ _) = xorIns ins where
  xorIns :: Instruction 'KXor -> IO ()
  xorIns _ = undefined
executeInstruction ins@(Bit _ _) = bitIns ins where
  bitIns :: Instruction 'KBit -> IO ()
  bitIns (Bit (Uimm3 Uimm3Nat) _) = undefined
executeInstruction ins@(Res _ _) = resIns ins where
  resIns :: Instruction 'KRes -> IO ()
  resIns _ = undefined
executeInstruction ins@(Set _ _) = setIns ins where
  setIns :: Instruction 'KSet -> IO ()
  setIns _ = undefined
executeInstruction ins@(Swap _) = swapIns ins where
  swapIns :: Instruction 'KSwap -> IO ()
  swapIns _ = undefined

someFunc :: IO ()
someFunc = do
  let instruction = Load (Reg8 RegA) (Reg8 RegB)
  executeInstruction instruction
