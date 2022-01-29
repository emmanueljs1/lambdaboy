{-# LANGUAGE GADTs, DataKinds, TypeFamilies #-}
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

data Operand :: * -> * where
  Imm8 :: Word8 -> Operand Word8
  Reg8 :: Reg r -> Operand Word8
  Imm16 :: Word16 -> Operand Word16
  Reg16 :: CanCombine r1 r2 ~ 'True => Reg r1 -> Reg r2 -> Operand Word16
  Indirect :: Operand Word16 -> Operand Word8

data Instruction where
  Load8 :: Operand Word8 -> Operand Word8 -> Instruction -- TODO: constrain to valid operands only?

someFunc :: IO ()
someFunc = putStrLn "someFunc"
