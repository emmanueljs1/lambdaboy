module CPUTests
  ( cpuTests
  )
where

import Control.Monad.ST
import Data.Array
import Data.Array.ST
import Data.Word
import Test.HUnit

import CPU
import Instruction
import Operand
import Registers

emptyCPU :: ST s (CPU (STArray s) (ST s))
emptyCPU = initCPU

withRegisters :: Registers -> ST s (CPU (STArray s) (ST s)) -> ST s (CPU (STArray s) (ST s))
withRegisters regs st = do
  cpu <- st
  return $ cpu { registers = regs }

withSP :: Word16 -> ST s (CPU (STArray s) (ST s)) -> ST s (CPU (STArray s) (ST s))
withSP w st = do
  cpu <- st
  return $ cpu { sp = w }

loadR8R8 :: Test
loadR8R8 = "LD r8, r8" ~: reg8 RegB (resultRegisters resultCPU) ~?= 0xF where
  resultCPU = runST $ do
    cpu <- withRegisters (setReg8 RegA 0xF emptyRegisters) emptyCPU
    cpu' <- load (Reg8 RegB) (Reg8 RegA) cpu
    toResultCPU cpu'

loadR8N8 :: Test
loadR8N8 = "LD r8, n8" ~: reg8 RegB (resultRegisters resultCPU) ~?= 0xF where
  resultCPU = runST $ do
    cpu <- emptyCPU
    cpu' <- load (Reg8 RegB) (Uimm8 0xF) cpu
    toResultCPU cpu'

loadR16N16 :: Test
loadR16N16 = "LD r16, n16" ~: reg16 RegB RegC (resultRegisters resultCPU) ~?= 0x1111 where
  resultCPU = runST $ do
    cpu <- emptyCPU
    cpu' <- load (Reg16 RegB RegC) (Uimm16 0x1111) cpu
    toResultCPU cpu'

loadIndirectHLR8 :: Test
loadIndirectHLR8 = "LD (HL), r8" ~: resultRAM resultCPU ! 0x1111 ~?= 0xF where
  resultCPU = runST $ do
    let regs = setReg16 RegH RegL 0x1111 (setReg8 RegC 0xF emptyRegisters)
    cpu <- withRegisters regs emptyCPU
    cpu' <- load (Indirect (Reg16 RegH RegL)) (Reg8 RegC) cpu
    toResultCPU cpu'

loadR8IndirectHL :: Test
loadR8IndirectHL = "LD r8, (HL)" ~: reg8 RegC (resultRegisters resultCPU) ~?= 0xF where
  resultCPU = runST $ do
    cpu <- withRegisters (setReg16 RegH RegL 0x1111 emptyRegisters) emptyCPU
    writeArray (ram cpu) 0x1111 0xF
    cpu' <- load (Reg8 RegC) (Indirect (Reg16 RegH RegL)) cpu
    toResultCPU cpu'

loadIndirectR16RA :: Test
loadIndirectR16RA = "LD (r16), A" ~: resultRAM resultCPU ! 0x1111 ~?= 0xF where
  resultCPU = runST $ do
    let regs = setReg8 RegA 0xF (setReg16 RegB RegC 0x1111 emptyRegisters)
    cpu <- withRegisters regs emptyCPU
    cpu' <- load (Indirect (Reg16 RegB RegC)) (Reg8 RegA) cpu
    toResultCPU cpu'

loadIndirectN16RA :: Test
loadIndirectN16RA = "LD (n16), A" ~: resultRAM resultCPU ! 0x1111 ~?= 0xF where
  resultCPU = runST $ do
    cpu <- withRegisters (setReg8 RegA 0xF emptyRegisters) emptyCPU
    cpu' <- load (Indirect (Uimm16 0x1111)) (Reg8 RegA) cpu
    toResultCPU cpu'

loadFF00N8RA :: Test
loadFF00N8RA = "LD ($FF00 + n8), A" ~: resultRAM resultCPU ! 0xFF01 ~?= 0xF where
  resultCPU = runST $ do
    cpu <- withRegisters (setReg8 RegA 0xF emptyRegisters) emptyCPU
    cpu' <- load (Indirect (FF00Offset (Uimm8 0x1))) (Reg8 RegA) cpu
    toResultCPU cpu'

loadFF00RCRA :: Test
loadFF00RCRA = "LD ($FF00 + C), A" ~: resultRAM resultCPU ! 0xFF01 ~?= 0xF where
  resultCPU = runST $ do
    let regs = setReg8 RegC 0x1 $ setReg8 RegA 0xF emptyRegisters
    cpu <- withRegisters regs emptyCPU
    cpu' <- load (Indirect (FF00Offset (Uimm8 0x1))) (Reg8 RegA) cpu
    toResultCPU cpu'

loadRAIndirectR16 :: Test
loadRAIndirectR16 = "LD A, (r16)" ~: reg8 RegA (resultRegisters resultCPU) ~?= 0xF where
  resultCPU = runST $ do
    cpu <- withRegisters (setReg16 RegB RegC 0x1111 emptyRegisters) emptyCPU
    writeArray (ram cpu) 0x1111 0xF
    cpu' <- load (Reg8 RegA) (Indirect (Reg16 RegB RegC)) cpu
    toResultCPU cpu'

loadRAIndirectN16 :: Test
loadRAIndirectN16 = "LD A, (n16)" ~: reg8 RegA (resultRegisters resultCPU) ~?= 0xF where
  resultCPU = runST $ do
    cpu <- emptyCPU
    writeArray (ram cpu) 0x1111 0xF
    cpu' <- load (Reg8 RegA) (Indirect (Uimm16 0x1111)) cpu
    toResultCPU cpu'

loadRAFF00N8 :: Test
loadRAFF00N8 = "LD A, ($FF00 + n8)" ~: reg8 RegA (resultRegisters resultCPU) ~?= 0xF where
  resultCPU = runST $ do
    cpu <- emptyCPU
    writeArray (ram cpu) 0xFF01 0xF
    cpu' <- load (Reg8 RegA) (Indirect (FF00Offset (Uimm8 0x1))) cpu
    toResultCPU cpu'


loadRAFF00RC :: Test
loadRAFF00RC = "LD A, ($FF00 + C)" ~: reg8 RegA (resultRegisters resultCPU) ~?= 0xF where
  resultCPU = runST $ do
    cpu <- withRegisters (setReg8 RegC 0x1 emptyRegisters) emptyCPU
    writeArray (ram cpu) 0xFF01 0xF
    cpu' <- load (Reg8 RegA) (Indirect (FF00Offset (Reg8 RegC))) cpu
    toResultCPU cpu'

loadIndirectHLIRA :: Test
loadIndirectHLIRA =
  "LD (HLI), A" ~: TestList [ "address update" ~: resultRAM resultCPU ! 0x1111 ~?= 0xF
                            , "post instruction" ~: reg16 RegH RegL (resultRegisters resultCPU) ~?= 0x1112
                            ] where
  resultCPU = runST $ do
    let regs = setReg16 RegH RegL 0x1111 (setReg8 RegA 0xF emptyRegisters)
    cpu <- withRegisters regs emptyCPU
    cpu' <- load (Indirect (PostInstruction (Reg16 RegH RegL) IncrementAfter)) (Reg8 RegA) cpu
    toResultCPU cpu'

loadIndirectHLDRA :: Test
loadIndirectHLDRA =
  "LD (HLI), A" ~: TestList [ "address update" ~: resultRAM resultCPU ! 0x1111 ~?= 0xF
                            , "post instruction" ~: reg16 RegH RegL (resultRegisters resultCPU) ~?= 0x1110
                            ] where
  resultCPU = runST $ do
    let regs = setReg16 RegH RegL 0x1111 (setReg8 RegA 0xF emptyRegisters)
    cpu <- withRegisters regs emptyCPU
    cpu' <- load (Indirect (PostInstruction (Reg16 RegH RegL) DecrementAfter)) (Reg8 RegA) cpu
    toResultCPU cpu'

loadRAIndirectHLI :: Test
loadRAIndirectHLI =
  "LD A, (HLI)" ~: TestList [ "register update" ~:  reg8 RegA (resultRegisters resultCPU) ~?= 0xF
                            , "post instruction" ~: reg16 RegH RegL (resultRegisters resultCPU) ~?= 0x1112
                            ] where
  resultCPU = runST $ do
    cpu <- withRegisters (setReg16 RegH RegL 0x1111 emptyRegisters) emptyCPU
    writeArray (ram cpu) 0x1111 0xF
    cpu' <- load (Reg8 RegA) (Indirect (PostInstruction (Reg16 RegH RegL) IncrementAfter)) cpu
    toResultCPU cpu'


loadRAIndirectHLD :: Test
loadRAIndirectHLD =
  "LD A, (HLD)" ~: TestList [ "register update" ~:  reg8 RegA (resultRegisters resultCPU) ~?= 0xF
                            , "post instruction" ~: reg16 RegH RegL (resultRegisters resultCPU) ~?= 0x1110
                            ] where
  resultCPU = runST $ do
    cpu <- withRegisters (setReg16 RegH RegL 0x1111 emptyRegisters) emptyCPU
    writeArray (ram cpu) 0x1111 0xF
    cpu' <- load (Reg8 RegA) (Indirect (PostInstruction (Reg16 RegH RegL) DecrementAfter)) cpu
    toResultCPU cpu'

loadSPN16 :: Test
loadSPN16 = "LD SP, n16" ~: resultSP resultCPU ~?= 0xFFFF where
  resultCPU = runST $ do
    cpu <- emptyCPU
    cpu' <- load (StackPointer Unchanged) (Uimm16 0xFFFF) cpu
    toResultCPU cpu'

loadIndirectN16SP :: Test
loadIndirectN16SP =
  "LD (n16), SP" ~: TestList [ "first addr update" ~: resultRAM resultCPU ! 0x1111 ~?= 0xFF
                             , "second addr update" ~: resultRAM resultCPU ! 0x1112 ~?= 0xFF
                             ] where
  resultCPU = runST $ do
    cpu <- withSP 0xFFFF emptyCPU
    cpu' <- load (Indirect (Uimm16 0x1111)) (StackPointer Unchanged) cpu
    toResultCPU cpu'

loadHLSPE8 :: Test
loadHLSPE8 =
  "LD HL, SP + e8" ~: TestList [ "registers update" ~: reg16 RegH RegL (resultRegisters resultCPU) ~?= 0x0000
                               , "c flag" ~: flagC (resultFlags resultCPU) ~?= True
                               , "h flag" ~: flagH (resultFlags resultCPU) ~?= True
                               ] where
  resultCPU = runST $ do
    cpu <- withSP 0xFFFF emptyCPU
    cpu' <- load (Reg16 RegH RegL) (StackPointer (AddInt8 1)) cpu
    toResultCPU cpu'

loadSPHL :: Test
loadSPHL = "LD SP, HL" ~: resultSP resultCPU ~?= 0xFFFF where
  resultCPU = runST $ do
    cpu <- withRegisters (setReg16 RegH RegL 0xFFFF emptyRegisters) emptyCPU
    cpu' <- load (StackPointer Unchanged) (Reg16 RegH RegL) cpu
    toResultCPU cpu'

loadTests :: Test
loadTests = "LD tests" ~: TestList [ loadR8R8
                                   , loadR8N8
                                   , loadR16N16
                                   , loadIndirectHLR8
                                   , loadR8IndirectHL
                                   , loadIndirectR16RA
                                   , loadIndirectN16RA
                                   , loadFF00N8RA
                                   , loadFF00RCRA
                                   , loadRAIndirectR16
                                   , loadRAIndirectN16
                                   , loadRAFF00N8
                                   , loadRAFF00RC
                                   , loadIndirectHLIRA
                                   , loadIndirectHLDRA
                                   , loadRAIndirectHLI
                                   , loadRAIndirectHLD
                                   , loadSPN16
                                   , loadIndirectN16SP
                                   , loadHLSPE8
                                   , loadSPHL
                                   ]

add8Expected :: ArithmeticType atk -> ResultCPU -> Test
add8Expected at resultCPU = TestList [ "register update" ~: reg8 RegA (resultRegisters resultCPU) ~?= 0x00 + carry at
                                     , "c flag" ~: flagC (resultFlags resultCPU) ~?= True
                                     , "h flag" ~: flagH (resultFlags resultCPU) ~?= True
                                     , "z flag" ~: flagZ (resultFlags resultCPU) ~?= True
                                     ] where
  carry :: ArithmeticType atk -> Word8
  carry WithCarryIncluded = 0x1
  carry WithoutCarryIncluded = 0x0

addRAR8 :: Test
addRAR8 = TestList [ "ADD A, r8" ~: add8Expected WithoutCarryIncluded (resultCPU WithoutCarryIncluded)
                   , "ADC A, r8" ~: add8Expected WithCarryIncluded (resultCPU WithCarryIncluded)
                   ] where
  resultCPU at = runST $ do
    let regs = setReg8 RegA 0xFF (setReg8 RegB 0x01 emptyRegisters)
    cpu <- withRegisters regs emptyCPU
    cpu' <- add at (Reg8 RegA) (Reg8 RegB) cpu
    toResultCPU cpu'

addRAIndirectHL :: Test
addRAIndirectHL = TestList [ "ADD A, (HL)" ~: add8Expected WithoutCarryIncluded (resultCPU WithoutCarryIncluded)
                           , "ADC A, (HL)" ~: add8Expected WithCarryIncluded (resultCPU WithCarryIncluded)
                           ] where
  resultCPU at = runST $ do
    let regs = setReg8 RegA 0xFF (setReg16 RegH RegL 0x1111 emptyRegisters)
    cpu <- withRegisters regs emptyCPU
    writeArray (ram cpu) 0x1111 0x1
    cpu' <- add at (Reg8 RegA) (Indirect (Reg16 RegH RegL)) cpu
    toResultCPU cpu'

addRAN8 :: Test
addRAN8 = TestList [ "ADD A, n8" ~: add8Expected WithoutCarryIncluded (resultCPU WithoutCarryIncluded)
                   , "ADC A, n8" ~: add8Expected WithCarryIncluded (resultCPU WithCarryIncluded)
                   ] where
  resultCPU at = runST $ do
    cpu <- withRegisters (setReg8 RegA 0xFF emptyRegisters) emptyCPU
    cpu' <- add at (Reg8 RegA) (Uimm8 0x1) cpu
    toResultCPU cpu'

addTests :: Test
addTests = "ADD / ADC tests" ~: TestList [ addRAR8
                                         , addRAIndirectHL
                                         , addRAN8
                                         ]

cpuTests :: Test
cpuTests = "cpuTests" ~: TestList [loadTests
                                  , addTests
                                  ]
