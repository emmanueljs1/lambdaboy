{-# LANGUAGE GADTs, DataKinds, TypeFamilies, RankNTypes #-}
module Gameboy
    ( someFunc
    ) where

import Data.Word

data RegType
  = A
  | B
  | C
  | D
  | E
  | H
  | L

type family CanCombine (r1 :: RegType) (r2 :: RegType) :: Bool where
  CanCombine 'B 'C = 'True
  CanCombine 'D 'E = 'True
  CanCombine 'H 'L = 'True
  CanCombine _ _ = 'False

data Reg :: RegType -> * where
  RegA :: Reg 'A
  RegB :: Reg 'B
  RegC :: Reg 'C
  RegD :: Reg 'D
  RegE :: Reg 'E
  RegH :: Reg 'H
  RegL :: Reg 'L

data OperandKind
  = KImm8
  | KReg8
  | KImm16
  | KReg16
  | KIndirect

data Operand :: OperandKind -> * -> * where
  Imm8 :: Word8 -> Operand 'KImm8 Word8
  Reg8 :: Reg r -> Operand 'KReg8 Word8
  Imm16 :: Word16 -> Operand 'KImm16 Word16
  Reg16 :: CanCombine r1 r2 ~ 'True => Reg r1 -> Reg r2 -> Operand 'KReg16 Word16
  Indirect :: Operand k Word16 -> Operand 'KIndirect Word8

type family Loadable8 (k1 :: OperandKind) (o2 :: OperandKind) :: Bool where
  Loadable8 'KReg8 'KReg8 = 'True
  Loadable8 _ _ = 'False

data Instruction where
  Load8 :: Loadable8 k1 k2 ~ 'True => Operand k1 Word8 -> Operand k2 Word8 -> Instruction

someFunc :: IO ()
someFunc = putStrLn "someFunc"
