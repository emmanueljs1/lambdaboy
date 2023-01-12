module Instruction
  ( LoadOperands
  , AddOperands
  , AndOperands
  , CompareOperands
  , InstructionKind (..)
  , Instruction (..)
  , ArithmeticType (..)
  , RotateType (..)
  , RotateDirection (..)
  , ShiftType (..)
  , ShiftDirection (..)
  , PostRetOperation (..)
  )
where

import Control.Monad
import Data.Kind
import GHC.TypeLits
import Test.QuickCheck

import Operand

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
  LoadOperands 'KReg16 'KUimm16 = 'KLoad
  -- LD (HL), r8
  LoadOperands 'KIndirectHL 'KReg8 = 'KLoad
  -- LD (HL), n8
  LoadOperands 'KIndirectHL 'KUimm8 = 'KLoad
  -- LD r8, (HL)
  LoadOperands 'KReg8 'KIndirectHL = 'KLoad
  -- LD (r16), A | LD (n16), A | LD ($FF00 + n8), A | LD ($FF00 + C), A | LD (HLI), A | LD (HLD), A
  LoadOperands 'KIndirect 'KRegisterA = 'KLoad
  -- LD A, (r16) | LD A, (n16) | LD A, ($FF00 + n8) | LD A, ($FF00 + C) | LD A, (HLI) | LD A, (HLD)
  LoadOperands 'KRegisterA 'KIndirect = 'KLoad
  -- LD SP, n16
  LoadOperands ('KStackPointer 'KUnchanged) 'KUimm16 = 'KLoad
  -- LD (n16), SP
  LoadOperands 'KIndirectUimm16 ('KStackPointer 'KUnchanged) = 'KLoad
  -- LD HL, SP + e8
  LoadOperands 'KRegisterHL ('KStackPointer 'KAddInt8) = 'KLoad
  -- LD SP, HL
  LoadOperands ('KStackPointer 'KUnchanged) 'KRegisterHL = 'KLoad
  LoadOperands _ _ = 'KInvalid

data ArithmeticTypeKind = KWithCarryIncluded | KWithoutCarryIncluded

data ArithmeticType :: ArithmeticTypeKind -> Type where
  WithCarryIncluded :: ArithmeticType 'KWithCarryIncluded
  WithoutCarryIncluded :: ArithmeticType 'KWithoutCarryIncluded

deriving instance Show (ArithmeticType atk)

type family AddOperands (atk :: ArithmeticTypeKind) (k1 :: OperandKind) (k2 :: OperandKind) :: InstructionKind where
  AddOperands _ 'KRegisterA 'KReg8 = 'KAdd
  AddOperands _ 'KRegisterA 'KIndirectHL = 'KAdd
  AddOperands _ 'KRegisterA 'KUimm8 = 'KAdd
  AddOperands 'KWithoutCarryIncluded 'KRegisterHL 'KReg16 = 'KAdd
  AddOperands 'KWithoutCarryIncluded 'KRegisterHL ('KStackPointer 'KUnchanged) = 'KAdd
  AddOperands 'KWithoutCarryIncluded ('KStackPointer 'KUnchanged) 'KImm8 = 'KAdd
  AddOperands _ _ _ = 'KInvalid

type family AndOperands (k1 :: OperandKind) (k2 :: OperandKind) :: InstructionKind where
  AndOperands 'KRegisterA 'KReg8 = 'KAnd
  AndOperands 'KRegisterA 'KIndirectHL = 'KAnd
  AndOperands 'KRegisterA 'KUimm8 = 'KAnd
  AndOperands _ _ = 'KInvalid

type family CompareOperands (k1 :: OperandKind) (k2 :: OperandKind) :: InstructionKind where
  CompareOperands 'KRegisterA 'KReg8 = 'KCompare
  CompareOperands 'KRegisterA 'KIndirectHL = 'KCompare
  CompareOperands 'KRegisterA 'KUimm8 = 'KCompare
  CompareOperands _ _ = 'KInvalid

type family DecrementOperand (ok :: OperandKind) :: InstructionKind where
  DecrementOperand 'KReg8 = 'KDecrement
  DecrementOperand 'KIndirectHL = 'KDecrement
  DecrementOperand 'KReg16 = 'KDecrement
  DecrementOperand ('KStackPointer 'KUnchanged) = 'KDecrement
  DecrementOperand _ = 'KInvalid

type family IncrementOperand (ok :: OperandKind) :: InstructionKind where
  IncrementOperand 'KReg8 = 'KIncrement
  IncrementOperand 'KIndirectHL = 'KIncrement
  IncrementOperand 'KReg16 = 'KIncrement
  IncrementOperand ('KStackPointer 'KUnchanged) = 'KIncrement
  IncrementOperand _ = 'KInvalid

type family OrOperands (k1 :: OperandKind) (k2 :: OperandKind) :: InstructionKind where
  OrOperands 'KRegisterA 'KReg8 = 'KOr
  OrOperands 'KRegisterA 'KIndirectHL = 'KOr
  OrOperands 'KRegisterA 'KUimm8 = 'KOr
  OrOperands _ _ = 'KInvalid

type family SubOperands (k1 :: OperandKind) (k2 :: OperandKind) :: InstructionKind where
  SubOperands 'KRegisterA 'KReg8 = 'KSub
  SubOperands 'KRegisterA 'KIndirectHL = 'KSub
  SubOperands 'KRegisterA 'KUimm8 = 'KSub
  SubOperands _ _ = 'KInvalid

type family XorOperands (k1 :: OperandKind) (k2 :: OperandKind) :: InstructionKind where
  XorOperands 'KRegisterA 'KReg8 = 'KXor
  XorOperands 'KRegisterA 'KIndirectHL = 'KXor
  XorOperands 'KRegisterA 'KUimm8 = 'KXor
  XorOperands _ _ = 'KInvalid

type family BitOperand (ok :: OperandKind) :: InstructionKind where
  BitOperand 'KReg8 = 'KBit
  BitOperand 'KIndirectHL = 'KBit
  BitOperand _ = 'KInvalid

type family ResOperand (ok :: OperandKind) :: InstructionKind where
  ResOperand 'KReg8 = 'KRes
  ResOperand 'KIndirectHL = 'KRes
  ResOperand _ = 'KInvalid

type family SetOperand (ok :: OperandKind) :: InstructionKind where
  SetOperand 'KReg8 = 'KSet
  SetOperand 'KIndirectHL = 'KSet
  SetOperand _ = 'KInvalid

type family SwapOperand (ok :: OperandKind) :: InstructionKind where
  SwapOperand 'KReg8 = 'KSwap
  SwapOperand 'KIndirectHL = 'KSwap
  SwapOperand _ = 'KInvalid

type family RotateOperand (ok :: OperandKind) :: InstructionKind where
  RotateOperand 'KReg8 = 'KRotate
  RotateOperand 'KIndirectHL = 'KRotate
  RotateOperand _ = 'KInvalid

type family ShiftOperands (k1 :: OperandKind) (k2 :: OperandKind) :: InstructionKind where
  ShiftOperands 'KReg8 'KIndirectHL = 'KShift
  ShiftOperands _ _ = 'KInvalid

data ShiftInstructionValidity = ValidShift

type family ShiftInstruction (dk :: ShiftDirectionKind) (tk :: ShiftTypeKind) :: ShiftInstructionValidity where
  ShiftInstruction 'KShiftRight _ = 'ValidShift
  ShiftInstruction 'KShiftLeft 'KShiftArithmetically = 'ValidShift

type family JumpOperand (ck :: ConditionCodeKind) (ok :: OperandKind) :: InstructionKind where
  JumpOperand 'KEmptyCode 'KRegisterHL = 'KJump
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
  PopOperand 'KReg16 = 'KPop
  PopOperand _ = 'KInvalid

type family PushOperand (ok :: OperandKind) :: InstructionKind where
  PushOperand 'KRegisterAF = 'KPush
  PushOperand 'KReg16 = 'KPush
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

data Instruction where
  Load :: LoadOperands k1 k2 ~ 'KLoad => Operand k1 -> Operand k2 -> Instruction
  Add :: AddOperands atk k1 k2 ~ 'KAdd => ArithmeticType atk -> Operand k1 -> Operand k2 -> Instruction
  And :: AndOperands k1 k2 ~ 'KAnd => Operand k1 -> Operand k2 -> Instruction
  Compare :: CompareOperands k1 k2 ~ 'KCompare => Operand k1 -> Operand k2 -> Instruction
  Decrement :: DecrementOperand ok ~ 'KDecrement => Operand ok -> Instruction
  Increment :: IncrementOperand ok ~ 'KIncrement => Operand ok -> Instruction
  Or :: OrOperands k1 k2 ~ 'KOr => Operand k1 -> Operand k2 -> Instruction
  Sub :: SubOperands k1 k2 ~ 'KSub => ArithmeticType atk -> Operand k1 -> Operand k2 -> Instruction
  Xor :: XorOperands k1 k2 ~ 'KXor => Operand k1 -> Operand k2 -> Instruction
  Bit :: (KnownNat n, BitOperand ok ~ 'KBit) => Uimm3 n -> Operand ok -> Instruction
  Res :: (KnownNat n, ResOperand ok ~ 'KRes) => Uimm3 n -> Operand ok -> Instruction
  Set :: (KnownNat n, SetOperand ok ~ 'KSet) => Uimm3 n -> Operand ok -> Instruction
  Swap :: SwapOperand ok ~ 'KSwap => Operand ok -> Instruction
  Rotate :: RotateOperand ok ~ 'KRotate => RotateDirection -> RotateType -> Operand ok -> Instruction
  Shift :: (ShiftOperands k1 k2 ~ 'KShift, ShiftInstruction dk tk ~ 'ValidShift) => ShiftDirection dk -> ShiftType tk -> Operand k1 -> Operand k2 -> Instruction
  Call :: ConditionCode ck -> Operand 'KUimm16 -> Instruction
  Jump :: JumpOperand ck ok ~ 'KJump => ConditionCode ck -> Operand ok -> Instruction
  Ret :: RetInstruction ck prok ~ 'ValidRetInstruction => ConditionCode ck -> PostRetOperation prok -> Instruction
  Rst :: KnownNat n => RstVector n -> Instruction
  Pop :: PopOperand ok ~ 'KPop => Operand ok -> Instruction
  Push :: PushOperand ok ~ 'KPush => Operand ok -> Instruction
  ComplementCarryFlag :: Instruction
  ComplementAcc :: Instruction
  DecimalAdjustAcc :: Instruction
  ToggleInterrupts :: Bool -> Instruction
  Halt :: Instruction
  Nop :: Instruction
  SetCarryFlag :: Instruction
  Stop :: Instruction

instance Show Instruction where
  show (Load o1 o2) = "LD " ++ show o1 ++ ", " ++ show o2
  show _ = undefined

instance Arbitrary Instruction where
  arbitrary = oneof [arbitraryLoad] where
    arbitraryLoad = oneof [ liftM2 Load (arbitrary :: Gen (Operand 'KReg8)) (arbitrary :: Gen (Operand 'KReg8))
                          , liftM2 Load (arbitrary :: Gen (Operand 'KReg8)) (arbitrary :: Gen (Operand 'KUimm8))
                          , liftM2 Load (arbitrary :: Gen (Operand 'KReg16)) (arbitrary :: Gen (Operand 'KUimm16))
                          , liftM2 Load (arbitrary :: Gen (Operand 'KIndirectHL)) (arbitrary :: Gen (Operand 'KReg8))
                          , liftM2 Load (arbitrary :: Gen (Operand 'KIndirectHL)) (arbitrary :: Gen (Operand 'KUimm8))
                          , liftM2 Load (arbitrary :: Gen (Operand 'KReg8)) (arbitrary :: Gen (Operand 'KIndirectHL))
                          , liftM2 Load (arbitrary :: Gen (Operand 'KIndirect)) (arbitrary :: Gen (Operand 'KRegisterA))
                          , liftM2 Load (arbitrary :: Gen (Operand 'KRegisterA)) (arbitrary :: Gen (Operand 'KIndirect))
                          , liftM2 Load (arbitrary :: Gen (Operand ('KStackPointer 'KUnchanged))) (arbitrary :: Gen (Operand 'KUimm16))
                          , liftM2 Load (arbitrary :: Gen (Operand 'KIndirectUimm16)) (arbitrary :: Gen (Operand ('KStackPointer 'KUnchanged)))
                          , liftM2 Load (arbitrary :: Gen (Operand 'KRegisterHL)) (arbitrary :: Gen (Operand ('KStackPointer 'KAddInt8)))
                          , liftM2 Load (arbitrary :: Gen (Operand ('KStackPointer 'KUnchanged))) (arbitrary :: Gen (Operand 'KRegisterHL))
                          ]
