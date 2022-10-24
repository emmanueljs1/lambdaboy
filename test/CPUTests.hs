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

withRegisters :: Registers -> ST s (CPU (STArray s) (ST s)) -> ST s (CPU (STArray s) (ST s))
withRegisters regs st = do
  cpu <- st
  return $ cpu { registers = regs }

loadR8R8 :: Test
loadR8R8 =
  let resultCPU = runST $ do
        cpu <- withRegisters (setReg8 RegA 0xF emptyRegisters) initCPU
        cpu' <- load (Reg8 RegB) (Reg8 RegA) cpu
        toResultCPU cpu'
 in reg8 RegB (resultRegisters resultCPU) ~?= 0xF

loadIndirectR16R8 :: Test
loadIndirectR16R8 =
  let resultCPU = runST $ do
        let regs = setReg16 RegH RegL 0x12 (setReg8 RegA 0xF emptyRegisters)
        cpu <- withRegisters regs initCPU
        cpu' <- load (Indirect (Reg16 RegH RegL)) (Reg8 RegA) cpu
        toResultCPU cpu'
  in resultRAM resultCPU ! 0x12 ~?= 0xF

loadTests :: Test
loadTests =
  TestList [ loadR8R8
           , loadIndirectR16R8
           ]

cpuTests :: Test
cpuTests = TestList [loadTests]
