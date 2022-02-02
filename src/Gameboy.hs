{-# LANGUAGE GADTs, DataKinds, TypeFamilies, StandaloneDeriving, ConstraintKinds, TypeOperators, UndecidableInstances, PolyKinds #-}
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

data ValidCombinedRegs = ValidCombinedRegs

type family CombinedRegs (r1 :: RegType) (r2 :: RegType) :: ValidCombinedRegs where
  CombinedRegs 'B 'C = 'ValidCombinedRegs
  CombinedRegs 'D 'E = 'ValidCombinedRegs
  CombinedRegs 'H 'L = 'ValidCombinedRegs
  CombinedRegs r1 r2 = TypeError ('Text "Combined regs " ':<>: 'ShowType r1 ':<>:
                                  'Text " and " ':<>: 'ShowType r2 ':<>:
                                  'Text " are not valid")

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
  | KBitToTest

type IndirectHLKind = 'KIndirect ('KReg16 'H 'L)

data Addressable = Addresable

type family Address (ok :: OperandKind) :: Addressable where
  Address 'KUimm16 = 'Addresable
  Address ('KReg16 _ _) = 'Addresable
  Address ok = TypeError ('Text "Operand does not represent valid address: " ':<>: 'ShowType ok)

data Offsetable = Offsetable OffsetType

type family Offset (ok :: OperandKind) :: Offsetable where
  Offset 'KUimm8 = 'Offsetable 'Uimm8Offset
  Offset ('KReg8 'C) = 'Offsetable 'RegCOffset
  Offset ok = TypeError ('Text "Operand does not represent valid offset: " ':<>: 'ShowType ok)

data StackPointerOperation :: StackPointerOperationKind -> * where
  Unchanged :: StackPointerOperation 'KUnchanged
  AddInt8 :: Int8 -> StackPointerOperation 'KAddInt8

deriving instance Show (StackPointerOperation spo)

data PostInstructionOperation :: PostInstructionOperationKind -> * where
  IncrementAfter :: PostInstructionOperation 'KIncrementAfter
  DecrementAfter :: PostInstructionOperation 'KDecrementAfter

deriving instance Show (PostInstructionOperation piok)

data PostOperable = PostOperable

type family PostOperation (ok :: OperandKind) (piok :: PostInstructionOperationKind) :: PostOperable where
  PostOperation ('KReg16 'H 'L) 'KIncrementAfter = 'PostOperable
  PostOperation ('KReg16 'H 'L) 'KDecrementAfter = 'PostOperable
  PostOperation ok piok = TypeError ('ShowType piok ':<>: 'Text " is not a valid post instruction operation for " ':<>: 'ShowType ok)

data BitToTest (n :: Nat) = BitToTest deriving Show

type Is3Bits (n :: Nat) = ((0 <=? n) ~ 'True, (n <=? 7) ~ 'True)

data Operand :: OperandKind -> * where
  Uimm8 :: Word8 -> Operand 'KUimm8
  Imm8 :: Int8 -> Operand 'KImm8
  Reg8 :: Reg r -> Operand ('KReg8 r)
  Uimm16 :: Word16 -> Operand 'KUimm16
  Reg16 :: CombinedRegs r1 r2 ~ 'ValidCombinedRegs => Reg r1 -> Reg r2 -> Operand ('KReg16 r1 r2)
  Indirect :: Address ok ~ 'Addresable => Operand ok -> Operand ('KIndirect ok)
  IndirectHL :: Operand IndirectHLKind
  FF00Offset :: Offset ok ~ 'Offsetable ot => Operand ok -> Operand ('KFF00Offset ot)
  StackPointer :: StackPointerOperation k -> Operand ('KStackPointer k)
  PostInstruction :: PostOperation ok piok ~ 'PostOperable => Operand ok -> PostInstructionOperation piok -> Operand ('KPostInstruction ok piok)
  TestBit :: Is3Bits n => BitToTest n -> Operand 'KBitToTest

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

type family OperandsTypeError (ins :: ErrorMessage) (k1 :: OperandKind) (k2 :: OperandKind) where
  OperandsTypeError ins k1 k2 =
    TypeError ('Text "Operands do not represent a valid " ':<>: ins ':<>: 'Text " instruction:" ':$$:
               ins ':<>: 'Text " " ':<>: 'ShowType k1 ':<>: 'Text ", " ':<>: 'ShowType k2)

type family OperandTypeError (ins :: ErrorMessage) (ok :: OperandKind) where
  OperandTypeError ins ok =
    TypeError ('Text "Operand does not represent a valid " ':<>: ins ':<>: 'Text " instruction:" ':$$:
               ins ':<>: 'Text " " ':<>: 'ShowType ok)

type family LoadOperands (k1 :: OperandKind) (k2 :: OperandKind) :: InstructionKind where
  -- LD r8, r8
  LoadOperands ('KReg8 _) ('KReg8 _) = 'KLoad
  -- LD r8, n8
  LoadOperands ('KReg8 _) 'KUimm8 = 'KLoad
  -- LD r16, n16
  LoadOperands ('KReg16 _ _) 'KUimm16 = 'KLoad
  -- LD (HL), r8
  LoadOperands IndirectHLKind ('KReg8 _) = 'KLoad
  -- LD (HL), n8
  LoadOperands IndirectHLKind 'KUimm8 = 'KLoad
  -- LD r8, (HL)
  LoadOperands ('KReg8 _) IndirectHLKind = 'KLoad
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
  LoadOperands ok1 ok2 = OperandsTypeError ('Text "LOAD") ok1 ok2

data ArithmeticTypeKind = KWithCarryIncluded | KWithoutCarryIncluded

data ArithmeticType :: ArithmeticTypeKind -> * where
  WithCarryIncluded :: ArithmeticType 'KWithCarryIncluded
  WithoutCarryIncluded :: ArithmeticType 'KWithoutCarryIncluded

type family AddOperands (atk :: ArithmeticTypeKind) (k1 :: OperandKind) (k2 :: OperandKind) :: InstructionKind where
  AddOperands _ ('KReg8 'A) ('KReg8 _) = 'KAdd
  AddOperands _ ('KReg8 'A) IndirectHLKind = 'KAdd
  AddOperands _ ('KReg8 'A) 'KUimm8 = 'KAdd
  AddOperands 'KWithoutCarryIncluded ('KReg16 'H 'L) ('KStackPointer 'KUnchanged) = 'KAdd
  AddOperands 'KWithoutCarryIncluded ('KStackPointer 'KUnchanged) 'KImm8 = 'KAdd
  AddOperands atk k1 k2 = OperandsTypeError ('Text "ADD" ':<>: 'ShowType atk) k1 k2

type family AndOperands (k1 :: OperandKind) (k2 :: OperandKind) :: InstructionKind where
  AndOperands ('KReg8 'A) ('KReg8 _) = 'KAnd
  AndOperands ('KReg8 'A) IndirectHLKind = 'KAnd
  AndOperands ('KReg8 'A) 'KUimm8 = 'KAnd
  AndOperands k1 k2 =  OperandsTypeError ('Text "AND") k1 k2

type family CompareOperands (k1 :: OperandKind) (k2 :: OperandKind) :: InstructionKind where
  CompareOperands ('KReg8 'A) ('KReg8 _) = 'KCompare
  CompareOperands ('KReg8 'A) IndirectHLKind = 'KCompare
  CompareOperands ('KReg8 'A) 'KUimm8 = 'KCompare
  CompareOperands k1 k2 = OperandsTypeError ('Text "COMPARE") k1 k2 

type family DecrementOperand (ok :: OperandKind) :: InstructionKind where
  DecrementOperand ('KReg8 _) = 'KDecrement
  DecrementOperand IndirectHLKind = 'KDecrement
  DecrementOperand ('KReg16 _ _) = 'KDecrement
  DecrementOperand ('KStackPointer 'KUnchanged) = 'KDecrement
  DecrementOperand ok = OperandTypeError ('Text "DEC") ok

type family IncrementOperand (ok :: OperandKind) :: InstructionKind where
  IncrementOperand ('KReg8 _) = 'KIncrement
  IncrementOperand IndirectHLKind = 'KIncrement
  IncrementOperand ('KReg16 _ _) = 'KIncrement
  IncrementOperand ('KStackPointer 'KUnchanged) = 'KIncrement
  IncrementOperand ok = OperandTypeError ('Text "INC") ok

type family OrOperands (k1 :: OperandKind) (k2 :: OperandKind) :: InstructionKind where
  OrOperands ('KReg8 'A) ('KReg8 _) = 'KOr
  OrOperands ('KReg8 'A) IndirectHLKind = 'KOr
  OrOperands ('KReg8 'A) 'KUimm8 = 'KOr
  OrOperands k1 k2 = OperandsTypeError ('Text "OR") k1 k2

type family SubOperands (k1 :: OperandKind) (k2 :: OperandKind) :: InstructionKind where
  SubOperands ('KReg8 'A) ('KReg8 _) = 'KSub
  SubOperands ('KReg8 'A) IndirectHLKind = 'KSub
  SubOperands ('KReg8 'A) 'KUimm8 = 'KSub
  SubOperands k1 k2 = OperandsTypeError ('Text "SUB") k1 k2

type family XorOperands (k1 :: OperandKind) (k2 :: OperandKind) :: InstructionKind where
  XorOperands ('KReg8 'A) ('KReg8 _) = 'KXor
  XorOperands ('KReg8 'A) IndirectHLKind = 'KXor
  XorOperands ('KReg8 'A) 'KUimm8 = 'KXor
  XorOperands k1 k2 = OperandsTypeError ('Text "XOR") k1 k2

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
  xorIns = undefined

someFunc :: IO ()
someFunc = do
  let instruction = Load (Reg8 RegA) (Reg8 RegB)
  executeInstruction instruction
