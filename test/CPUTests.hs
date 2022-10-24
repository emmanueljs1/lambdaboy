module CPUTests
  ( cpuTests
  )
where

import Control.Monad.ST
import Data.Array
import Data.Array.ST
import Test.HUnit

import CPU
import Operand
import Registers

emptyCPU :: ST s (CPU (STArray s) (ST s))
emptyCPU = initCPU

withRegisters :: Registers -> ST s (CPU (STArray s) (ST s)) -> ST s (CPU (STArray s) (ST s))
withRegisters regs st = do
  cpu <- st
  return $ cpu { registers = regs }

loadR8R8 :: Test
loadR8R8 =
  let regs = runST $ do
        cpu <- withRegisters (setReg8 RegA 0xF emptyRegisters) emptyCPU
        cpu' <- load (Reg8 RegB) (Reg8 RegA) cpu
        return $ registers cpu'
 in reg8 RegB regs ~?= 0xF

loadIndirectR16R8 :: Test
loadIndirectR16R8 =
  let arr = runSTArray $ do
        let regs = setReg16 RegH RegL 0x12 (setReg8 RegA 0xF emptyRegisters)
        cpu <- withRegisters regs emptyCPU
        cpu' <- load (Indirect (Reg16 RegH RegL)) (Reg8 RegA) cpu
        return $ ram cpu'
  in arr ! 0x12 ~?= 0xF


loadTests :: Test
loadTests =
  TestList [ loadR8R8
           , loadIndirectR16R8
           ]

cpuTests :: Test
cpuTests = TestList [loadTests]
