module Instruction
  ( LoadOperands
  , AddOperands
  , AndOperands
  , InstructionKind (..)
  , Instruction (..)
  , Ins (..)
  , ArithmeticType (..)
  , RotateType (..)
  , RotateDirection (..)
  , ShiftType (..)
  , ShiftDirection (..)
  , PostRetOperation (..)
  )
where

import Data.Kind
import GHC.TypeLits

import Operand
import Registers

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
  | KRotate
  | KShift
  | KCall
  | KJump
  | KRet
  | KRst
  | KPop
  | KPush
  | KComplementCarryFlag
  | KComplementAcc
  | KDecimalAdjustAcc
  | KToggleInterrupts
  | KHalt
  | KNop
  | KSetCarryFlag
  | KStop
  | KInvalid

type family LoadOperands (k1 :: OperandKind) (k2 :: OperandKind) :: InstructionKind where
  -- LD r8, r8
  LoadOperands 'KReg8 'KReg8 = 'KLoad
  -- LD r8, n8
  LoadOperands 'KReg8 'KUimm8 = 'KLoad
  -- LD r16, n16
  LoadOperands ('KReg16 _ _) 'KUimm16 = 'KLoad
  -- LD (HL), r8
  LoadOperands ('KIndirect ('KReg16 'H 'L)) 'KReg8 = 'KLoad
  -- LD (HL), n8
  LoadOperands ('KIndirect ('KReg16 'H 'L)) 'KUimm8 = 'KLoad
  -- LD r8, (HL)
  LoadOperands 'KReg8 ('KIndirect ('KReg16 'H 'L)) = 'KLoad
  -- LD (r16), A
  LoadOperands ('KIndirect ('KReg16 _ _)) 'KRegisterA = 'KLoad
  -- LD (n16), A
  LoadOperands ('KIndirect 'KUimm16) 'KRegisterA = 'KLoad
  -- LD ($FF00 + n8), A | LD ($FF00 + C), A
  LoadOperands ('KIndirect ('KFF00Offset _)) 'KRegisterA = 'KLoad
  -- LD A, (r16)
  LoadOperands 'KRegisterA ('KIndirect ('KReg16 _ _)) = 'KLoad
  -- LD A, (n16)
  LoadOperands 'KRegisterA ('KIndirect 'KUimm16) = 'KLoad
  -- LD A, ($FF00 + n8) | LD A, ($FF00 + C)
  LoadOperands 'KRegisterA ('KIndirect ('KFF00Offset _)) = 'KLoad
  -- LD (HLI), A | LD (HLD), A
  LoadOperands ('KIndirect ('KPostInstruction ('KReg16 'H 'L))) 'KRegisterA = 'KLoad
  -- LD A, (HLI) | LD A, (HLD)
  LoadOperands 'KRegisterA ('KIndirect ('KPostInstruction ('KReg16 'H 'L))) = 'KLoad
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

data ArithmeticType :: ArithmeticTypeKind -> Type where
  WithCarryIncluded :: ArithmeticType 'KWithCarryIncluded
  WithoutCarryIncluded :: ArithmeticType 'KWithoutCarryIncluded

deriving instance Show (ArithmeticType atk)

type family AddOperands (atk :: ArithmeticTypeKind) (k1 :: OperandKind) (k2 :: OperandKind) :: InstructionKind where
  AddOperands _ 'KRegisterA 'KReg8 = 'KAdd
  AddOperands _ 'KRegisterA ('KIndirect ('KReg16 'H 'L)) = 'KAdd
  AddOperands _ 'KRegisterA 'KUimm8 = 'KAdd
  AddOperands 'KWithoutCarryIncluded ('KReg16 'H 'L) ('KReg16 _ _) = 'KAdd
  AddOperands 'KWithoutCarryIncluded ('KReg16 'H 'L) ('KStackPointer 'KUnchanged) = 'KAdd
  AddOperands 'KWithoutCarryIncluded ('KStackPointer 'KUnchanged) 'KImm8 = 'KAdd
  AddOperands _ _ _ = 'KInvalid

type family AndOperands (k1 :: OperandKind) (k2 :: OperandKind) :: InstructionKind where
  AndOperands 'KRegisterA 'KReg8 = 'KAnd
  AndOperands 'KRegisterA ('KIndirect ('KReg16 'H 'L)) = 'KAnd
  AndOperands 'KRegisterA 'KUimm8 = 'KAnd
  AndOperands _ _ = 'KInvalid

type family CompareOperands (k1 :: OperandKind) (k2 :: OperandKind) :: InstructionKind where
  CompareOperands 'KRegisterA 'KReg8 = 'KCompare
  CompareOperands 'KRegisterA ('KIndirect ('KReg16 'H 'L)) = 'KCompare
  CompareOperands 'KRegisterA 'KUimm8 = 'KCompare
  CompareOperands _ _ = 'KInvalid

type family DecrementOperand (ok :: OperandKind) :: InstructionKind where
  DecrementOperand 'KReg8 = 'KDecrement
  DecrementOperand ('KIndirect ('KReg16 'H 'L)) = 'KDecrement
  DecrementOperand ('KReg16 _ _) = 'KDecrement
  DecrementOperand ('KStackPointer 'KUnchanged) = 'KDecrement
  DecrementOperand _ = 'KInvalid

type family IncrementOperand (ok :: OperandKind) :: InstructionKind where
  IncrementOperand 'KReg8 = 'KIncrement
  IncrementOperand ('KIndirect ('KReg16 'H 'L)) = 'KIncrement
  IncrementOperand ('KReg16 _ _) = 'KIncrement
  IncrementOperand ('KStackPointer 'KUnchanged) = 'KIncrement
  IncrementOperand _ = 'KInvalid

type family OrOperands (k1 :: OperandKind) (k2 :: OperandKind) :: InstructionKind where
  OrOperands 'KRegisterA 'KReg8 = 'KOr
  OrOperands 'KRegisterA ('KIndirect ('KReg16 'H 'L)) = 'KOr
  OrOperands 'KRegisterA 'KUimm8 = 'KOr
  OrOperands _ _ = 'KInvalid

type family SubOperands (k1 :: OperandKind) (k2 :: OperandKind) :: InstructionKind where
  SubOperands 'KRegisterA 'KReg8 = 'KSub
  SubOperands 'KRegisterA ('KIndirect ('KReg16 'H 'L)) = 'KSub
  SubOperands 'KRegisterA 'KUimm8 = 'KSub
  SubOperands _ _ = 'KInvalid

type family XorOperands (k1 :: OperandKind) (k2 :: OperandKind) :: InstructionKind where
  XorOperands 'KRegisterA 'KReg8 = 'KXor
  XorOperands 'KRegisterA ('KIndirect ('KReg16 'H 'L)) = 'KXor
  XorOperands 'KRegisterA 'KUimm8 = 'KXor
  XorOperands _ _ = 'KInvalid

type family BitOperand (ok :: OperandKind) :: InstructionKind where
  BitOperand 'KReg8 = 'KBit
  BitOperand ('KIndirect ('KReg16 'H 'L)) = 'KBit
  BitOperand _ = 'KInvalid

type family ResOperand (ok :: OperandKind) :: InstructionKind where
  ResOperand 'KReg8 = 'KRes
  ResOperand ('KIndirect ('KReg16 'H 'L)) = 'KRes
  ResOperand _ = 'KInvalid

type family SetOperand (ok :: OperandKind) :: InstructionKind where
  SetOperand 'KReg8 = 'KSet
  SetOperand ('KIndirect ('KReg16 'H 'L)) = 'KSet
  SetOperand _ = 'KInvalid

type family SwapOperand (ok :: OperandKind) :: InstructionKind where
  SwapOperand 'KReg8 = 'KSwap
  SwapOperand ('KIndirect ('KReg16 'H 'L)) = 'KSwap
  SwapOperand _ = 'KInvalid

type family RotateOperand (ok :: OperandKind) :: InstructionKind where
  RotateOperand 'KReg8 = 'KRotate
  RotateOperand ('KIndirect ('KReg16 'H 'L)) = 'KRotate
  RotateOperand _ = 'KInvalid

type family ShiftOperands (k1 :: OperandKind) (k2 :: OperandKind) :: InstructionKind where
  ShiftOperands 'KReg8 ('KIndirect ('KReg16 'H 'L)) = 'KShift
  ShiftOperands _ _ = 'KInvalid

data ShiftInstructionValidity = ValidShift

type family ShiftInstruction (dk :: ShiftDirectionKind) (tk :: ShiftTypeKind) :: ShiftInstructionValidity where
  ShiftInstruction 'KShiftRight _ = 'ValidShift
  ShiftInstruction 'KShiftLeft 'KShiftArithmetically = 'ValidShift

type family JumpOperand (ck :: ConditionCodeKind) (ok :: OperandKind) :: InstructionKind where
  JumpOperand 'KEmptyCode ('KIndirect ('KReg16 'H 'L)) = 'KJump
  JumpOperand _ 'KUimm16 = 'KJump
  JumpOperand _ 'KImm8 = 'KJump
  JumpOperand _ _ = 'KInvalid

data RetInstructionValidity = ValidRetInstruction | InvalidRetInstruction

type family RetInstruction (ck :: ConditionCodeKind) (prok :: PostRetOperationKind) :: RetInstructionValidity where
  RetInstruction _ 'KPostRetNoop = 'ValidRetInstruction
  RetInstruction 'KEmptyCode 'KPostRetEnableInterrupts = 'ValidRetInstruction
  RetInstruction _ _ = 'InvalidRetInstruction

type family PopOperand (ok :: OperandKind) :: InstructionKind where
  PopOperand 'KRegisterAF = 'KPop
  PopOperand ('KReg16 _ _) = 'KPop
  PopOperand _ = 'KInvalid

type family PushOperand (ok :: OperandKind) :: InstructionKind where
  PushOperand 'KRegisterAF = 'KPush
  PushOperand ('KReg16 _ _) = 'KPush
  PushOperand _ = 'KInvalid

data RotateType = DefaultRotate | ThroughCarry
  deriving Show

data RotateDirection = RotateRight | RotateLeft
  deriving Show

data ShiftDirectionKind = KShiftRight | KShiftLeft

data ShiftDirection :: ShiftDirectionKind -> Type where
  ShiftRight :: ShiftDirection 'KShiftRight
  ShiftLeft :: ShiftDirection 'KShiftLeft

deriving instance Show (ShiftDirection sdk)

data ShiftTypeKind = KShiftLogically | KShiftArithmetically

data ShiftType :: ShiftTypeKind -> Type where
  ShiftLogically :: ShiftType 'KShiftLogically
  ShiftArithmetically :: ShiftType 'KShiftArithmetically

deriving instance Show (ShiftType stk)

data PostRetOperationKind
  = KPostRetNoop
  | KPostRetEnableInterrupts

data PostRetOperation :: PostRetOperationKind -> Type where
  PostRetNoop :: PostRetOperation 'KPostRetNoop
  PostRetEnableInterrupts :: PostRetOperation 'KPostRetEnableInterrupts

deriving instance Show (PostRetOperation prok)

data Ins where
  Ins :: Instruction k -> Ins

data Instruction :: InstructionKind -> Type where
  Load :: LoadOperands k1 k2 ~ 'KLoad => Operand k1 -> Operand k2 -> Instruction 'KLoad
  Add :: AddOperands atk k1 k2 ~ 'KAdd => ArithmeticType atk -> Operand k1 -> Operand k2 -> Instruction 'KAdd
  And :: AndOperands k1 k2 ~ 'KAnd => Operand k1 -> Operand k2 -> Instruction 'KAnd
  Compare :: CompareOperands k1 k2 ~ 'KCompare => Operand k1 -> Operand k2 -> Instruction 'KCompare
  Decrement :: DecrementOperand ok ~ 'KDecrement => Operand ok -> Instruction 'KDecrement
  Increment :: IncrementOperand ok ~ 'KIncrement => Operand ok -> Instruction 'KIncrement
  Or :: OrOperands k1 k2 ~ 'KOr => Operand k1 -> Operand k2 -> Instruction 'KOr
  Sub :: SubOperands k1 k2 ~ 'KSub => ArithmeticType atk -> Operand k1 -> Operand k2 -> Instruction 'KSub
  Xor :: XorOperands k1 k2 ~ 'KXor => Operand k1 -> Operand k2 -> Instruction 'KXor
  Bit :: (KnownNat n, BitOperand ok ~ 'KBit) => Uimm3 n -> Operand ok -> Instruction 'KBit
  Res :: (KnownNat n, ResOperand ok ~ 'KRes) => Uimm3 n -> Operand ok -> Instruction 'KRes
  Set :: (KnownNat n, SetOperand ok ~ 'KSet) => Uimm3 n -> Operand ok -> Instruction 'KSet
  Swap :: SwapOperand ok ~ 'KSwap => Operand ok -> Instruction 'KSwap
  Rotate :: RotateOperand ok ~ 'KRotate => RotateDirection -> RotateType -> Operand ok -> Instruction 'KRotate
  Shift :: (ShiftOperands k1 k2 ~ 'KShift, ShiftInstruction dk tk ~ 'ValidShift) => ShiftDirection dk -> ShiftType tk -> Operand k1 -> Operand k2 -> Instruction 'KShift
  Call :: ConditionCode ck -> Operand 'KUimm16 -> Instruction 'KCall
  Jump :: JumpOperand ck ok ~ 'KJump => ConditionCode ck -> Operand ok -> Instruction 'KJump
  Ret :: RetInstruction ck prok ~ 'ValidRetInstruction => ConditionCode ck -> PostRetOperation prok -> Instruction 'KRet
  Rst :: KnownNat n => RstVector n -> Instruction 'KRst
  Pop :: PopOperand ok ~ 'KPop => Operand ok -> Instruction 'KPop
  Push :: PushOperand ok ~ 'KPush => Operand ok -> Instruction 'KPush
  ComplementCarryFlag :: Instruction 'KComplementCarryFlag
  ComplementAcc :: Instruction 'KComplementAcc
  DecimalAdjustAcc :: Instruction 'KDecimalAdjustAcc
  ToggleInterrupts :: Bool -> Instruction 'KToggleInterrupts
  Halt :: Instruction 'KHalt
  Nop :: Instruction 'KNop
  SetCarryFlag :: Instruction 'KSetCarryFlag
  Stop :: Instruction 'KStop

deriving instance Show (Instruction ik)
