module Operand
  ( PostInstructionOperation (..)
  , PostOperableValidity (..)
  , PostOperable
  , StackPointerOperation (..)
  , StackPointerOperationKind (..)
  , Offsetable
  , OperandKind (..)
  , Operand (..)
  , RstVector (..)
  , ConditionCodeKind (..)
  , ConditionCode (..)
  , Uimm3 (..)
  , offsetFF00
  , postInstruction
  , offsetSP
  )
where

import Data.Int
import Data.Kind
import Data.Word
import GHC.TypeLits

import Registers

data StackPointerOperationKind = KUnchanged | KAddInt8

data OperandKind
  = KUimm8
  | KImm8
  | KReg8
  | KUimm16
  | KReg16 RegType RegType
  | KIndirect OperandKind
  | KFF00Offset OperandKind
  | KStackPointer StackPointerOperationKind
  | KPostInstruction OperandKind
  | KRegisterAF
  | KRegisterA
  | KRegisterC

data AddressValidity = ValidAddress | InvalidAddress

type family Addressable (ok :: OperandKind) :: AddressValidity where
  Addressable 'KUimm16 = 'ValidAddress
  Addressable ('KReg16 _ _) = 'ValidAddress
  Addressable ('KFF00Offset _) = 'ValidAddress
  Addressable ('KPostInstruction ('KReg16 'H 'L)) = 'ValidAddress
  Addressable _ = 'InvalidAddress

data OffsetValidity = ValidOffset | InvalidOffset

type family Offsetable (ok :: OperandKind) :: OffsetValidity where
  Offsetable 'KUimm8 = 'ValidOffset
  Offsetable 'KRegisterC = 'ValidOffset
  Offsetable _ = 'InvalidOffset

data StackPointerOperation :: StackPointerOperationKind -> Type where
  Unchanged :: StackPointerOperation 'KUnchanged
  AddInt8 :: Int8 -> StackPointerOperation 'KAddInt8

deriving instance Show (StackPointerOperation spo)

data PostInstructionOperation = IncrementAfter
                              | DecrementAfter
                              deriving Show

data PostOperableValidity = ValidPostOperable | InvalidPostOperable

type family PostOperable (ok :: OperandKind) :: PostOperableValidity where
  PostOperable ('KReg16 'H 'L) = 'ValidPostOperable
  PostOperable _ = 'InvalidPostOperable

data Operand :: OperandKind -> Type where
  Uimm8 :: Word8 -> Operand 'KUimm8
  Imm8 :: Int8 -> Operand 'KImm8
  Reg8 :: Reg r -> Operand 'KReg8
  Uimm16 :: Word16 -> Operand 'KUimm16
  Reg16 :: CombinedRegs r1 r2 ~ 'RegsCompatible => Reg r1 -> Reg r2 -> Operand ('KReg16 r1 r2)
  Indirect :: Addressable ok ~ 'ValidAddress => Operand ok -> Operand ('KIndirect ok)
  FF00Offset :: Offsetable ok ~ 'ValidOffset => Operand ok -> Operand ('KFF00Offset ok)
  StackPointer :: StackPointerOperation k -> Operand ('KStackPointer k)
  PostInstruction :: PostOperable ok ~ 'ValidPostOperable => Operand ok -> PostInstructionOperation -> Operand ('KPostInstruction ok)
  RegisterAF :: Operand 'KRegisterAF
  RegisterA :: Operand 'KRegisterA
  RegisterC :: Operand 'KRegisterC 

deriving instance Show (Operand ok)

offsetFF00 :: Offsetable ok ~ 'ValidOffset => Operand ok -> Registers -> Word16
offsetFF00 (Uimm8 n8) _ = 0xFF00 + fromIntegral n8
offsetFF00 RegisterC regs = 0xFF00 + fromIntegral (reg8 RegC regs)

postInstruction :: PostOperable ok ~ 'ValidPostOperable => Operand ('KPostInstruction ok) -> Registers -> Registers
postInstruction (PostInstruction (Reg16 RegH RegL) operation) regs =
  let opFun = case operation of
               IncrementAfter -> (+1)
               DecrementAfter -> (\n -> n - 1)
  in setReg16 RegH RegL (opFun (reg16 RegH RegL regs)) regs

offsetSP :: StackPointerOperation 'KAddInt8 -> Word16 -> Word16
offsetSP (AddInt8 e8) =
  if e8 < 0 then subtract (fromIntegral (-1 * e8)) else (+ fromIntegral e8)

type Only3Bits (n :: Nat) = (KnownNat n, 0 <= n, n <= 7)

data Uimm3 :: Nat -> Type where
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

data ConditionCode :: ConditionCodeKind -> Type where
  CodeZ :: ConditionCode 'KCodeZ
  CodeNZ :: ConditionCode 'KCodeNZ
  CodeC :: ConditionCode 'KCodeC
  CodeNC :: ConditionCode 'KCodeNC
  NegateCode :: ConditionCode 'KNegateCode
  EmptyCode :: ConditionCode 'KEmptyCode

deriving instance Show (ConditionCode cck)

data RstVectorValidity = ValidRstVector

type family RstVectorType (n :: Nat):: RstVectorValidity where
  RstVectorType 0x00 = 'ValidRstVector
  RstVectorType 0x08 = 'ValidRstVector
  RstVectorType 0x10 = 'ValidRstVector
  RstVectorType 0x18 = 'ValidRstVector
  RstVectorType 0x20 = 'ValidRstVector
  RstVectorType 0x28 = 'ValidRstVector
  RstVectorType 0x30 = 'ValidRstVector
  RstVectorType 0x38 = 'ValidRstVector

data RstVector :: Nat -> Type where
  RstVector :: (KnownNat n, RstVectorType n ~ 'ValidRstVector) => RstVector n

instance KnownNat n => Show (RstVector n) where
  show b = "RstVector " ++ show (natVal b)
