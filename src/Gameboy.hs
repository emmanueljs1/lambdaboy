{-# LANGUAGE GADTs, DataKinds, TypeFamilies, StandaloneDeriving, ConstraintKinds, TypeOperators #-}
module Gameboy
    ( someFunc
    )
where

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
  | KRegAF

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
  Reg16 :: CombinedRegs r1 r2 ~ 'RegsCompatible => Reg r1 -> Reg r2 -> Operand ('KReg16 r1 r2)
  Indirect :: Address ok ~ 'Addresable => Operand ok -> Operand ('KIndirect ok)
  FF00Offset :: Offset ok ~ 'Offsetable ot => Operand ok -> Operand ('KFF00Offset ot)
  StackPointer :: StackPointerOperation k -> Operand ('KStackPointer k)
  PostInstruction :: PostOperation ok piok ~ 'PostOperable => Operand ok -> PostInstructionOperation piok -> Operand ('KPostInstruction ok piok)
  RegAF :: Operand 'KRegAF

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

data Uimm3 (n :: Nat) = Uimm3

instance KnownNat n => Show (Uimm3 n) where
  show b = "BitToTest " ++ show (natVal b)

type Only3Bits (n :: Nat) = (KnownNat n, 0 <= n, n <= 7)

type family BitOperands (ok :: OperandKind) :: InstructionKind where
  BitOperands ('KReg8 _) = 'KBit
  BitOperands ('KIndirect ('KReg16 'H 'L)) = 'KBit
  BitOperands _ = 'KInvalid

type family ResOperands (ok :: OperandKind) :: InstructionKind where
  ResOperands ('KReg8 _) = 'KRes
  ResOperands ('KIndirect ('KReg16 'H 'L)) = 'KRes
  ResOperands  _ = 'KInvalid

type family SetOperands (ok :: OperandKind) :: InstructionKind where
  SetOperands ('KReg8 _) = 'KSet
  SetOperands ('KIndirect ('KReg16 'H 'L)) = 'KSet
  SetOperands _ = 'KInvalid

type family SwapOperand (ok :: OperandKind) :: InstructionKind where
  SwapOperand ('KReg8 _) = 'KSwap
  SwapOperand ('KIndirect ('KReg16 'H 'L)) = 'KSwap
  SwapOperand _ = 'KInvalid

data RotateType = DefaultRotate | ThroughCarry

data RotateDirection = RotateRight | RotateLeft

type family RotateOperand (ok :: OperandKind) :: InstructionKind where
  RotateOperand ('KReg8 _) = 'KRotate
  RotateOperand ('KIndirect ('KReg16 'H 'L)) = 'KRotate
  RotateOperand _ = 'KInvalid

data ShiftDirectionKind = KShiftRight | KShiftLeft

data ShiftDirection :: ShiftDirectionKind -> * where
  ShiftRight :: ShiftDirection 'KShiftRight
  ShiftLeft :: ShiftDirection 'KShiftLeft

data ShiftTypeKind = KShiftLogically | KShiftArithmetically

data ShiftType :: ShiftTypeKind -> * where
  ShiftLogically :: ShiftType 'KShiftLogically
  ShiftArithmetically :: ShiftType 'KShiftArithmetically

type family ShiftOperands (k1 :: OperandKind) (k2 :: OperandKind) :: InstructionKind where
  ShiftOperands ('KReg8 _) ('KIndirect ('KReg16 'H 'L)) = 'KShift
  ShiftOperands _ _ = 'KInvalid

data ShiftInstructionValidity = ValidShift | InvalidShift

type family ShiftInstruction (dk :: ShiftDirectionKind) (tk :: ShiftTypeKind) :: ShiftInstructionValidity where
  ShiftInstruction 'KShiftRight _ = 'ValidShift
  ShiftInstruction 'KShiftLeft 'KShiftArithmetically = 'ValidShift
  ShiftInstruction _ _ = 'InvalidShift

data ConditionCodeKind
  = KCodeZ
  | KCodeNZ
  | KCodeC
  | KCodeNC
  | KNegateCode
  | KEmptyCode

data ConditionCode :: ConditionCodeKind -> * where
  CodeZ :: ConditionCode 'KCodeZ
  CodeNZ :: ConditionCode 'KCodeNZ
  CodeC :: ConditionCode 'KCodeC
  CodeNC :: ConditionCode 'KCodeNC
  NegateCode :: ConditionCode 'KNegateCode
  EmptyCode :: ConditionCode 'KEmptyCode

type family JumpOperand (ck :: ConditionCodeKind) (ok :: OperandKind) :: InstructionKind where
  JumpOperand 'KEmptyCode ('KIndirect ('KReg16 'H 'L)) = 'KJump
  JumpOperand _ 'KUimm16 = 'KJump
  JumpOperand _ 'KImm8 = 'KJump
  JumpOperand _ _ = 'KInvalid

data PostRetOperationKind
  = KPostRetNoop
  | KPostRetEnableInterrupts

data PostRetOperation :: PostRetOperationKind -> * where
  PostRetNoop :: PostRetOperation 'KPostRetNoop
  PostRetEnableInterrupts :: PostRetOperation 'KPostRetEnableInterrupts

data RetInstructionValidity = ValidRetInstruction | InvalidRetInstruction

type family RetInstruction (ck :: ConditionCodeKind) (prok :: PostRetOperationKind) :: RetInstructionValidity where
  RetInstruction _ 'KPostRetNoop = 'ValidRetInstruction
  RetInstruction 'KEmptyCode 'KPostRetEnableInterrupts = 'ValidRetInstruction
  RetInstruction _ _ = 'InvalidRetInstruction

data RstVectorValidity = ValidRstVector | InvalidRstVector

data RstVector (n :: Nat) = RstVector

instance KnownNat n => Show (RstVector n) where
  show b = "RstVector " ++ show (natVal b)

type family RstVectorType (n :: Nat)  :: RstVectorValidity where
  RstVectorType 0x00 = 'ValidRstVector
  RstVectorType 0x08 = 'ValidRstVector
  RstVectorType 0x10 = 'ValidRstVector
  RstVectorType 0x18 = 'ValidRstVector
  RstVectorType 0x20 = 'ValidRstVector
  RstVectorType 0x28 = 'ValidRstVector
  RstVectorType 0x30 = 'ValidRstVector
  RstVectorType 0x38 = 'ValidRstVector
  RstVectorType _ = 'InvalidRstVector

type family PopOperand (ok :: OperandKind) :: InstructionKind where
  PopOperand 'KRegAF = 'KPop
  PopOperand ('KReg16 _ _) = 'KPop
  PopOperand _ = 'KInvalid

type family PushOperand (ok :: OperandKind) :: InstructionKind where
  PushOperand 'KRegAF = 'KPush
  PushOperand ('KReg16 _ _) = 'KPush
  PushOperand _ = 'KInvalid

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
  Bit :: (BitOperands ok ~ 'KBit, Only3Bits n) => Uimm3 n -> Operand ok -> Instruction 'KBit
  Res :: (ResOperands ok ~ 'KRes, Only3Bits n) => Uimm3 n -> Operand ok -> Instruction 'KRes
  Set :: (SetOperands ok ~ 'KSet, Only3Bits n) => Uimm3 n -> Operand ok -> Instruction 'KSet
  Swap :: SwapOperand ok ~ 'KSwap => Operand ok -> Instruction 'KSwap
  Rotate :: RotateOperand ok ~ 'KRotate => RotateDirection -> RotateType -> Operand ok -> Instruction 'KRotate
  Shift :: (ShiftOperands k1 k2 ~ 'KShift, ShiftInstruction dk tk ~'ValidShift) => ShiftDirection dk -> ShiftType tk -> Operand k1 -> Operand k2 -> Instruction 'KShift
  Call :: ConditionCode ck -> Operand 'KUimm16 -> Instruction 'KCall
  Jump :: JumpOperand ck ok ~ 'KJump => ConditionCode ck -> Operand ok -> Instruction 'KJump
  Ret :: RetInstruction ck prok ~ 'ValidRetInstruction => ConditionCode ck -> PostRetOperation prok -> Instruction 'KRet
  Rst :: RstVectorType n ~ 'ValidRstVector => RstVector n -> Instruction 'KRst
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

-- TODO: implement
executeInstruction :: Instruction k -> IO ()
executeInstruction ins@(Load {}) = loadIns ins where
  loadIns :: Instruction 'KLoad -> IO ()
  loadIns _ = undefined
executeInstruction ins@(Add {}) = addIns ins where
  addIns :: Instruction 'KAdd -> IO ()
  addIns (Add WithCarryIncluded _ _) = undefined
  addIns (Add WithoutCarryIncluded _ _) = undefined
executeInstruction ins@(And {}) = andIns ins where
  andIns :: Instruction 'KAnd -> IO ()
  andIns _ = undefined
executeInstruction ins@(Compare {}) = compareIns ins where
  compareIns :: Instruction 'KCompare -> IO ()
  compareIns _ = undefined
executeInstruction ins@(Decrement _) = decrementIns ins where
  decrementIns :: Instruction 'KDecrement -> IO ()
  decrementIns = undefined
executeInstruction ins@(Increment _) = incrementIns ins where
  incrementIns :: Instruction 'KIncrement -> IO ()
  incrementIns _ = undefined
executeInstruction ins@(Or {}) = orIns ins where
  orIns :: Instruction 'KOr -> IO ()
  orIns = undefined
executeInstruction ins@(Sub {}) = subIns ins where
  subIns :: Instruction 'KSub -> IO ()
  subIns = undefined
executeInstruction ins@(Xor {}) = xorIns ins where
  xorIns :: Instruction 'KXor -> IO ()
  xorIns _ = undefined
executeInstruction ins@(Bit {}) = bitIns ins where
  bitIns :: Instruction 'KBit -> IO ()
  bitIns (Bit Uimm3 _) = undefined
executeInstruction ins@(Res {}) = resIns ins where
  resIns :: Instruction 'KRes -> IO ()
  resIns _ = undefined
executeInstruction ins@(Set {}) = setIns ins where
  setIns :: Instruction 'KSet -> IO ()
  setIns _ = undefined
executeInstruction ins@(Swap _) = swapIns ins where
  swapIns :: Instruction 'KSwap -> IO ()
  swapIns _ = undefined
executeInstruction ins@(Rotate {}) = rotateIns ins where
  rotateIns :: Instruction 'KRotate -> IO ()
  rotateIns (Rotate RotateRight DefaultRotate _) = undefined
  rotateIns (Rotate RotateLeft ThroughCarry _) = undefined
  rotateIns _ = undefined
executeInstruction ins@(Shift {}) = shiftIns ins where
  shiftIns :: Instruction 'KShift -> IO ()
  shiftIns (Shift ShiftRight ShiftLogically _ _) = undefined
  shiftIns (Shift ShiftLeft ShiftArithmetically _ _) = undefined
  shiftIns _ = undefined
executeInstruction ins@(Call {}) = callIns ins where
  callIns :: Instruction 'KCall -> IO ()
  callIns (Call CodeZ _) = undefined
  callIns (Call CodeNZ _) = undefined
  callIns (Call CodeC _) = undefined
  callIns (Call CodeNC _) = undefined
  callIns (Call NegateCode _) = undefined
  callIns (Call EmptyCode _) = undefined
executeInstruction ins@(Jump {}) = jumpIns ins where
  jumpIns :: Instruction 'KJump -> IO ()
  jumpIns _ = undefined
executeInstruction ins@(Ret {}) = retIns ins where
  retIns :: Instruction 'KRet -> IO ()
  retIns (Ret _ PostRetNoop) = undefined
  retIns (Ret _ PostRetEnableInterrupts) = undefined
executeInstruction ins@(Rst {}) = rstIns ins where
  rstIns :: Instruction 'KRst -> IO ()
  rstIns (Rst RstVector) = undefined
executeInstruction ins@(Pop {}) = popIns ins where
  popIns :: Instruction 'KPop -> IO ()
  popIns _ = undefined
executeInstruction ins@(Push {}) = pushIns ins where
  pushIns :: Instruction 'KPush -> IO ()
  pushIns _ = undefined
executeInstruction ins@ComplementCarryFlag = ccfIns ins where
  ccfIns :: Instruction 'KComplementCarryFlag -> IO ()
  ccfIns _ = undefined
executeInstruction ins@ComplementAcc = cplIns ins where
  cplIns :: Instruction 'KComplementAcc -> IO ()
  cplIns _ = undefined
executeInstruction ins@DecimalAdjustAcc = daaIns ins where
  daaIns :: Instruction 'KDecimalAdjustAcc -> IO ()
  daaIns _ = undefined
executeInstruction ins@(ToggleInterrupts {}) = toggleInterruptsIns ins where
  toggleInterruptsIns :: Instruction 'KToggleInterrupts -> IO ()
  toggleInterruptsIns _ = undefined
executeInstruction ins@Halt = haltIns ins where
  haltIns :: Instruction 'KHalt -> IO ()
  haltIns _ = undefined
executeInstruction ins@Nop = nopIns ins where
  nopIns :: Instruction 'KNop -> IO ()
  nopIns _ = undefined
executeInstruction ins@SetCarryFlag = scfIns ins where
  scfIns :: Instruction 'KSetCarryFlag -> IO ()
  scfIns _ = undefined
executeInstruction ins@Stop = stopIns ins where
  stopIns :: Instruction 'KStop -> IO ()
  stopIns _ = undefined

someFunc :: IO ()
someFunc = do
  let instruction = Load (Reg8 RegA) (Reg8 RegB)
  executeInstruction instruction
