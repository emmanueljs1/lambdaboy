module Operand
  ( StackPointerOperation (..)
  , StackPointerOperationKind (..)
  , Offsetable
  , OperandKind (..)
  , Operand (..)
  , RstVector (..)
  , ConditionCodeKind (..)
  , ConditionCode (..)
  , Uimm3 (..)
  , offsetFF00
  , offsetSP
  )
where

import Data.Int
import Data.Kind
import Data.Word
import GHC.TypeLits
import Test.QuickCheck
import Type.Reflection

import Registers

data StackPointerOperationKind = KUnchanged | KAddInt8

data OperandKind
  = KUimm8
  | KImm8
  | KReg8
  | KUimm16
  | KReg16
  | KIndirect
  | KIndirectHL
  | KIndirectUimm16
  | KFF00Offset
  | KStackPointer StackPointerOperationKind
  | KRegisterHLI
  | KRegisterHLD
  | KRegisterAF
  | KRegisterHL
  | KRegisterA
  | KRegisterC

data AddressValidity = ValidAddress | InvalidAddress

type family Addressable (ok :: OperandKind) :: AddressValidity where
  Addressable 'KUimm16 = 'ValidAddress
  Addressable 'KReg16 = 'ValidAddress
  Addressable 'KRegisterHL = 'ValidAddress
  Addressable 'KFF00Offset = 'ValidAddress
  Addressable 'KRegisterHLI = 'ValidAddress
  Addressable 'KRegisterHLD = 'ValidAddress
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

data Operand ok where
  Uimm8 :: Word8 -> Operand 'KUimm8
  Imm8 :: Int8 -> Operand 'KImm8
  Reg8 :: Typeable r => Reg r -> Operand 'KReg8
  Uimm16 :: Word16 -> Operand 'KUimm16
  Reg16 :: (Typeable r1, Typeable r2, CombinedRegs r1 r2 ~ 'RegsCompatible) => Reg r1 -> Reg r2 -> Operand 'KReg16
  Indirect :: Addressable ok ~ 'ValidAddress => Operand ok -> Operand 'KIndirect
  FF00Offset :: Offsetable ok ~ 'ValidOffset => Operand ok -> Operand 'KFF00Offset
  StackPointer :: StackPointerOperation k -> Operand ('KStackPointer k)
  RegisterHLI :: Operand 'KRegisterHLI
  RegisterHLD :: Operand 'KRegisterHLD
  RegisterHL :: Operand 'KRegisterHL
  RegisterAF :: Operand 'KRegisterAF
  RegisterA :: Operand 'KRegisterA
  RegisterC :: Operand 'KRegisterC 
  IndirectHL :: Operand 'KIndirectHL
  IndirectUimm16 :: Word16 -> Operand 'KIndirectUimm16

instance Show (Operand ok) where
  show (Uimm8 n8) = show n8
  show (Imm8 n8) = show n8
  show (Reg8 r) = show r
  show (Uimm16 n16) = show n16
  show (Reg16 r1 r2) = show r1 ++ show r2
  show (Indirect op) = "(" ++ show op ++ ")"
  show (FF00Offset op) = "FF00 + " ++ show op
  show (StackPointer Unchanged) = "SP"
  show (StackPointer (AddInt8 i8)) = "SP" ++ (if i8 > 0 then "+" else "") ++ show i8
  show RegisterHLI = "HLI"
  show RegisterHLD = "HLD"
  show RegisterAF = "AF"
  show RegisterHL = "HL"
  show RegisterA = "A"
  show RegisterC = "C"
  show IndirectHL = show $ Indirect RegisterHL
  show (IndirectUimm16 n16) = show $ Indirect (Uimm16 n16)

instance Arbitrary (Operand 'KReg8) where
  arbitrary = elements [ Reg8 RegA
                       , Reg8 RegB
                       , Reg8 RegC
                       , Reg8 RegD
                       , Reg8 RegE
                       , Reg8 RegH
                       , Reg8 RegL
                       ]

instance Arbitrary (Operand 'KReg16) where
  arbitrary = elements [ Reg16 RegB RegC
                       , Reg16 RegD RegE
                       , Reg16 RegH RegL
                       ]

instance Arbitrary (Operand 'KRegisterA) where
  arbitrary = return RegisterA

instance Arbitrary (Operand 'KRegisterHL) where
  arbitrary = return RegisterHL

instance Arbitrary (Operand 'KIndirectHL) where
  arbitrary = return IndirectHL

instance Arbitrary (Operand 'KFF00Offset) where
  arbitrary = oneof [ return $ FF00Offset RegisterC
                    , FF00Offset <$> (arbitrary :: Gen (Operand 'KUimm8))
                    ]

instance Arbitrary (Operand 'KRegisterHLI) where
  arbitrary = return RegisterHLI

instance Arbitrary (Operand 'KRegisterHLD) where
  arbitrary = return RegisterHLD

instance Arbitrary (Operand 'KIndirect) where
  arbitrary = oneof [ Indirect <$> (arbitrary :: Gen (Operand 'KUimm16))
                    , Indirect <$> (arbitrary :: Gen (Operand 'KReg16))
                    , Indirect <$> (arbitrary :: Gen (Operand 'KRegisterHL))
                    , Indirect <$> (arbitrary :: Gen (Operand 'KFF00Offset))
                    , Indirect <$> (arbitrary :: Gen (Operand 'KRegisterHLI))
                    , Indirect <$> (arbitrary :: Gen (Operand 'KRegisterHLD))
                    ]

instance Arbitrary (Operand ('KStackPointer 'KUnchanged)) where
  arbitrary = return $ StackPointer Unchanged

instance Arbitrary (Operand ('KStackPointer 'KAddInt8)) where
  arbitrary = StackPointer . AddInt8 <$> arbitrary

instance Arbitrary (Operand 'KIndirectUimm16) where
  arbitrary = IndirectUimm16 <$> arbitrary

instance Arbitrary (Operand 'KUimm16) where
  arbitrary = Uimm16 <$> arbitrary

instance Arbitrary (Operand 'KUimm8) where
  arbitrary = Uimm8 <$> arbitrary

offsetFF00 :: Offsetable ok ~ 'ValidOffset => Operand ok -> Registers -> Word16
offsetFF00 (Uimm8 n8) _ = 0xFF00 + fromIntegral n8
offsetFF00 RegisterC regs = 0xFF00 + fromIntegral (reg8 RegC regs)

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
