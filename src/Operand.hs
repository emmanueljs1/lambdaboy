module Operand
  ( PostInstructionOperation (..)
  , PostInstructionOperationKind (..)
  , StackPointerOperation (..)
  , StackPointerOperationKind (..)
  , OperandKind (..)
  , Operand (..)
  , RstVector (..)
  , ConditionCodeKind (..)
  , ConditionCode (..)
  , Uimm3 (..)
  )
where

import Data.Int
import Data.Word
import GHC.TypeLits

import Registers

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

type Only3Bits (n :: Nat) = (KnownNat n, 0 <= n, n <= 7)

data Uimm3 :: Nat -> * where
  Uimm3 :: Only3Bits n => Uimm3 n

instance KnownNat n => Show (Uimm3 n) where
  show b = "Uimm3 " ++ show (natVal b)

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

deriving instance Show (ConditionCode cck)

data RstVectorValidity = ValidRstVector | InvalidRstVector

type family RstVectorType (n :: Nat):: RstVectorValidity where
  RstVectorType 0x00 = 'ValidRstVector
  RstVectorType 0x08 = 'ValidRstVector
  RstVectorType 0x10 = 'ValidRstVector
  RstVectorType 0x18 = 'ValidRstVector
  RstVectorType 0x20 = 'ValidRstVector
  RstVectorType 0x28 = 'ValidRstVector
  RstVectorType 0x30 = 'ValidRstVector
  RstVectorType 0x38 = 'ValidRstVector
  RstVectorType _ = 'InvalidRstVector

data RstVector :: Nat -> * where
  RstVector :: (KnownNat n, RstVectorType n ~ 'ValidRstVector) => RstVector n

instance KnownNat n => Show (RstVector n) where
  show b = "RstVector " ++ show (natVal b)
