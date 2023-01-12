module CPUTests
  ( cpuTests
  )
where

import Prelude hiding (and)

import Control.Monad.State.Lazy
import Control.Monad.ST
import Data.Array.ST
import Data.Word
import Test.HUnit
import Test.QuickCheck

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

prop_load :: Instruction 'KLoad -> FrozenCPU -> Bool
prop_load ins@(Load op1 op2) frozenCPU = checkLoad op1 op2 where
  frozenCPU' = runST $ do
    cpu :: CPU (STArray s) (ST s) <- fromFrozenCPU frozenCPU
    cpu' <- execStateT (executeInstruction ins) cpu
    freezeCPU cpu'
  checkLoad :: LoadOperands k1 k2 ~ 'KLoad => Operand k1 -> Operand k2 -> Bool
  checkLoad _ (StackPointer _) = True
  checkLoad o1 o2 = evalOp frozenCPU o2 False == evalOp frozenCPU' o1 True

loadR8N8 :: Test
loadR8N8 = "LD r8, n8" ~: reg8 RegB (frozenRegisters frozenCPU) ~?= 0xF where
  frozenCPU = runST $ do
    cpu <- emptyCPU
    cpu' <- execStateT (load (Reg8 RegB) (Uimm8 0xF)) cpu
    freezeCPU cpu'

loadR16N16 :: Test
loadR16N16 = "LD r16, n16" ~: reg16 RegB RegC (frozenRegisters frozenCPU) ~?= 0x1111 where
  frozenCPU = runST $ do
    cpu <- emptyCPU
    cpu' <- execStateT (load (Reg16 RegB RegC) (Uimm16 0x1111)) cpu
    freezeCPU cpu'

loadIndirectHLR8 :: Test
loadIndirectHLR8 = "LD (HL), r8" ~: readFrozenRAM (frozenRAM frozenCPU) 0x1111 ~?= 0xF where
  frozenCPU = runST $ do
    let regs = setReg16 RegH RegL 0x1111 (setReg8 RegC 0xF initRegisters)
    cpu <- withRegisters regs emptyCPU
    cpu' <- execStateT (load IndirectHL (Reg8 RegC)) cpu
    freezeCPU cpu'

loadR8IndirectHL :: Test
loadR8IndirectHL = "LD r8, (HL)" ~: reg8 RegC (frozenRegisters frozenCPU) ~?= 0xF where
  frozenCPU = runST $ do
    cpu <- withRegisters (setReg16 RegH RegL 0x1111 initRegisters) emptyCPU
    writeRAM (ram cpu) 0x1111 0xF
    cpu' <- execStateT (load (Reg8 RegC) IndirectHL) cpu
    freezeCPU cpu'

loadIndirectR16RA :: Test
loadIndirectR16RA = "LD (r16), A" ~: readFrozenRAM (frozenRAM frozenCPU) 0x1111 ~?= 0xF where
  frozenCPU = runST $ do
    let regs = setReg8 RegA 0xF (setReg16 RegB RegC 0x1111 initRegisters)
    cpu <- withRegisters regs emptyCPU
    cpu' <- execStateT (load (Indirect (Reg16 RegB RegC)) RegisterA) cpu
    freezeCPU cpu'

loadIndirectN16RA :: Test
loadIndirectN16RA = "LD (n16), A" ~: readFrozenRAM (frozenRAM frozenCPU) 0x1111 ~?= 0xF where
  frozenCPU = runST $ do
    cpu <- withRegisters (setReg8 RegA 0xF initRegisters) emptyCPU
    cpu' <- execStateT (load (Indirect (Uimm16 0x1111)) RegisterA) cpu
    freezeCPU cpu'

loadFF00N8RA :: Test
loadFF00N8RA = "LD ($FF00 + n8), A" ~: readFrozenRAM (frozenRAM frozenCPU) 0xFF01 ~?= 0xF where
  frozenCPU = runST $ do
    cpu <- withRegisters (setReg8 RegA 0xF initRegisters) emptyCPU
    cpu' <- execStateT (load (Indirect (FF00Offset (Uimm8 0x1))) RegisterA) cpu
    freezeCPU cpu'

loadFF00RCRA :: Test
loadFF00RCRA = "LD ($FF00 + C), A" ~: readFrozenRAM (frozenRAM frozenCPU) 0xFF01 ~?= 0xF where
  frozenCPU = runST $ do
    let regs = setReg8 RegC 0x1 $ setReg8 RegA 0xF initRegisters
    cpu <- withRegisters regs emptyCPU
    cpu' <- execStateT (load (Indirect (FF00Offset (Uimm8 0x1))) RegisterA) cpu
    freezeCPU cpu'

loadRAIndirectR16 :: Test
loadRAIndirectR16 = "LD A, (r16)" ~: reg8 RegA (frozenRegisters frozenCPU) ~?= 0xF where
  frozenCPU = runST $ do
    cpu <- withRegisters (setReg16 RegB RegC 0x1111 initRegisters) emptyCPU
    writeRAM (ram cpu) 0x1111 0xF
    cpu' <- execStateT (load RegisterA (Indirect (Reg16 RegB RegC))) cpu
    freezeCPU cpu'

loadRAIndirectN16 :: Test
loadRAIndirectN16 = "LD A, (n16)" ~: reg8 RegA (frozenRegisters frozenCPU) ~?= 0xF where
  frozenCPU = runST $ do
    cpu <- emptyCPU
    writeRAM (ram cpu) 0x1111 0xF
    cpu' <- execStateT (load RegisterA (Indirect (Uimm16 0x1111))) cpu
    freezeCPU cpu'

loadRAFF00N8 :: Test
loadRAFF00N8 = "LD A, ($FF00 + n8)" ~: reg8 RegA (frozenRegisters frozenCPU) ~?= 0xF where
  frozenCPU = runST $ do
    cpu <- emptyCPU
    writeRAM (ram cpu) 0xFF01 0xF
    cpu' <- execStateT (load RegisterA (Indirect (FF00Offset (Uimm8 0x1)))) cpu
    freezeCPU cpu'


loadRAFF00RC :: Test
loadRAFF00RC = "LD A, ($FF00 + C)" ~: reg8 RegA (frozenRegisters frozenCPU) ~?= 0xF where
  frozenCPU = runST $ do
    cpu <- withRegisters (setReg8 RegC 0x1 initRegisters) emptyCPU
    writeRAM (ram cpu) 0xFF01 0xF
    cpu' <- execStateT (load RegisterA (Indirect (FF00Offset RegisterC))) cpu
    freezeCPU cpu'

loadIndirectHLIRA :: Test
loadIndirectHLIRA =
  "LD (HLI), A" ~: TestList [ "address update" ~: readFrozenRAM (frozenRAM frozenCPU) 0x1111 ~?= 0xF
                            , "post instruction" ~: reg16 RegH RegL (frozenRegisters frozenCPU) ~?= 0x1112
                            ] where
  frozenCPU = runST $ do
    let regs = setReg16 RegH RegL 0x1111 (setReg8 RegA 0xF initRegisters)
    cpu <- withRegisters regs emptyCPU
    cpu' <- execStateT (load (Indirect RegisterHLI) RegisterA) cpu
    freezeCPU cpu'

loadIndirectHLDRA :: Test
loadIndirectHLDRA =
  "LD (HLD), A" ~: TestList [ "address update" ~: readFrozenRAM (frozenRAM frozenCPU) 0x1111 ~?= 0xF
                            , "post instruction" ~: reg16 RegH RegL (frozenRegisters frozenCPU) ~?= 0x1110
                            ] where
  frozenCPU = runST $ do
    let regs = setReg16 RegH RegL 0x1111 (setReg8 RegA 0xF initRegisters)
    cpu <- withRegisters regs emptyCPU
    cpu' <- execStateT (load (Indirect RegisterHLD) RegisterA) cpu
    freezeCPU cpu'

loadRAIndirectHLI :: Test
loadRAIndirectHLI =
  "LD A, (HLI)" ~: TestList [ "register update" ~:  reg8 RegA (frozenRegisters frozenCPU) ~?= 0xF
                            , "post instruction" ~: reg16 RegH RegL (frozenRegisters frozenCPU) ~?= 0x1112
                            ] where
  frozenCPU = runST $ do
    cpu <- withRegisters (setReg16 RegH RegL 0x1111 initRegisters) emptyCPU
    writeRAM (ram cpu) 0x1111 0xF
    cpu' <- execStateT (load RegisterA (Indirect RegisterHLI)) cpu
    freezeCPU cpu'


loadRAIndirectHLD :: Test
loadRAIndirectHLD =
  "LD A, (HLD)" ~: TestList [ "register update" ~:  reg8 RegA (frozenRegisters frozenCPU) ~?= 0xF
                            , "post instruction" ~: reg16 RegH RegL (frozenRegisters frozenCPU) ~?= 0x1110
                            ] where
  frozenCPU = runST $ do
    cpu <- withRegisters (setReg16 RegH RegL 0x1111 initRegisters) emptyCPU
    writeRAM (ram cpu) 0x1111 0xF
    cpu' <- execStateT (load RegisterA (Indirect RegisterHLD)) cpu
    freezeCPU cpu'

loadSPN16 :: Test
loadSPN16 = "LD SP, n16" ~: frozenSP frozenCPU ~?= 0xFFFF where
  frozenCPU = runST $ do
    cpu <- emptyCPU
    cpu' <- execStateT (load (StackPointer Unchanged) (Uimm16 0xFFFF)) cpu
    freezeCPU cpu'

loadIndirectN16SP :: Test
loadIndirectN16SP =
  "LD (n16), SP" ~: TestList [ "first addr update" ~: readFrozenRAM (frozenRAM frozenCPU) 0x1111 ~?= 0xFF
                             , "second addr update" ~: readFrozenRAM (frozenRAM frozenCPU) 0x1112 ~?= 0xFF
                             ] where
  frozenCPU = runST $ do
    cpu <- withSP 0xFFFF emptyCPU
    cpu' <- execStateT (load (IndirectUimm16 0x1111) (StackPointer Unchanged)) cpu
    freezeCPU cpu'

loadHLSPE8 :: Test
loadHLSPE8 =
  "LD HL, SP + e8" ~: TestList [ "registers update" ~: reg16 RegH RegL (frozenRegisters frozenCPU) ~?= 0x0000
                               , "c flag" ~: flagC (frozenFlags frozenCPU) ~?= True
                               , "h flag" ~: flagH (frozenFlags frozenCPU) ~?= True
                               ] where
  frozenCPU = runST $ do
    cpu <- withSP 0xFFFF emptyCPU
    cpu' <- execStateT (load RegisterHL (StackPointer (AddInt8 1))) cpu
    freezeCPU cpu'

loadSPHL :: Test
loadSPHL = "LD SP, HL" ~: frozenSP frozenCPU ~?= 0xFFFF where
  frozenCPU = runST $ do
    cpu <- withRegisters (setReg16 RegH RegL 0xFFFF initRegisters) emptyCPU
    cpu' <- execStateT (load (StackPointer Unchanged) RegisterHL) cpu
    freezeCPU cpu'

loadTests :: Test
loadTests = "LD tests" ~: TestList [ loadR8N8
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

add8Expected :: ArithmeticType atk -> FrozenCPU -> Test
add8Expected at frozenCPU = TestList [ "register update" ~: reg8 RegA (frozenRegisters frozenCPU) ~?= 0x00 + carry at
                                     , "c flag" ~: flagC (frozenFlags frozenCPU) ~?= True
                                     , "h flag" ~: flagH (frozenFlags frozenCPU) ~?= True
                                     , "z flag" ~: flagZ (frozenFlags frozenCPU) ~?= True
                                     ] where
  carry :: ArithmeticType atk -> Word8
  carry WithCarryIncluded = 0x1
  carry WithoutCarryIncluded = 0x0

addRAR8 :: Test
addRAR8 = TestList [ "ADD A, r8" ~: add8Expected WithoutCarryIncluded (frozenCPU WithoutCarryIncluded)
                   , "ADC A, r8" ~: add8Expected WithCarryIncluded (frozenCPU WithCarryIncluded)
                   ] where
  frozenCPU at = runST $ do
    let regs = setReg8 RegA 0xFF (setReg8 RegB 0x01 initRegisters)
    cpu <- withRegisters regs emptyCPU
    cpu' <- execStateT (add at RegisterA (Reg8 RegB)) cpu
    freezeCPU cpu'

addRAIndirectHL :: Test
addRAIndirectHL = TestList [ "ADD A, (HL)" ~: add8Expected WithoutCarryIncluded (frozenCPU WithoutCarryIncluded)
                           , "ADC A, (HL)" ~: add8Expected WithCarryIncluded (frozenCPU WithCarryIncluded)
                           ] where
  frozenCPU at = runST $ do
    let regs = setReg8 RegA 0xFF (setReg16 RegH RegL 0x1111 initRegisters)
    cpu <- withRegisters regs emptyCPU
    writeRAM (ram cpu) 0x1111 0x1
    cpu' <- execStateT (add at RegisterA IndirectHL) cpu
    freezeCPU cpu'

addRAN8 :: Test
addRAN8 = TestList [ "ADD A, n8" ~: add8Expected WithoutCarryIncluded (frozenCPU WithoutCarryIncluded)
                   , "ADC A, n8" ~: add8Expected WithCarryIncluded (frozenCPU WithCarryIncluded)
                   ] where
  frozenCPU at = runST $ do
    cpu <- withRegisters (setReg8 RegA 0xFF initRegisters) emptyCPU
    cpu' <- execStateT (add at RegisterA (Uimm8 0x1)) cpu
    freezeCPU cpu'

addHLR16 :: Test
addHLR16 = "ADD HL, r16" ~: TestList [ "registers updated" ~: reg16 RegH RegL (frozenRegisters frozenCPU) ~?= 0x0000
                                    , "c flag" ~: flagC (frozenFlags frozenCPU) ~?= True
                                    , "h flag" ~: flagH (frozenFlags frozenCPU) ~?= True
                                    , "z flag" ~: flagZ (frozenFlags frozenCPU) ~?= False
                                    , "n flag" ~: flagN (frozenFlags frozenCPU) ~?= False
                                    ] where
  frozenCPU = runST $ do
    let regs = setReg16 RegH RegL 0xFFFF (setReg16 RegB RegC 0x0001 initRegisters)
    cpu <- withRegisters regs emptyCPU
    cpu' <- execStateT (add WithoutCarryIncluded RegisterHL (Reg16 RegB RegC)) cpu
    freezeCPU cpu'

addHLSP :: Test
addHLSP = "ADD HL, r16" ~: TestList [ "registers updated" ~: reg16 RegH RegL (frozenRegisters frozenCPU) ~?= 0x0000
                                    , "c flag" ~: flagC (frozenFlags frozenCPU) ~?= True
                                    , "h flag" ~: flagH (frozenFlags frozenCPU) ~?= True
                                    , "z flag" ~: flagZ (frozenFlags frozenCPU) ~?= False
                                    , "n flag" ~: flagN (frozenFlags frozenCPU) ~?= False
                                    ] where
  frozenCPU = runST $ do
    let regs = setReg16 RegH RegL 0xFFFF initRegisters
    cpu <- withSP 0x0001 $ withRegisters regs emptyCPU
    cpu' <- execStateT (add WithoutCarryIncluded RegisterHL (StackPointer Unchanged)) cpu
    freezeCPU cpu'

addSPE8 :: Test
addSPE8 =
  "ADD SP, e8" ~: TestList [ "sp update" ~: frozenSP frozenCPU ~?= 0x0000
                           , "c flag" ~: flagC (frozenFlags frozenCPU) ~?= True
                           , "h flag" ~: flagH (frozenFlags frozenCPU) ~?= True
                           ] where
  frozenCPU = runST $ do
    cpu <- withSP 0xFFFF emptyCPU
    cpu' <- execStateT (add WithoutCarryIncluded (StackPointer Unchanged) (Imm8 1)) cpu
    freezeCPU cpu'

addTests :: Test
addTests = "ADD / ADC tests" ~: TestList [ addRAR8
                                         , addRAIndirectHL
                                         , addRAN8
                                         , addHLR16
                                         , addHLSP
                                         , addSPE8
                                         ]

andRAR8 :: Test
andRAR8 = "AND A, R8" ~: TestList [ "register updated" ~: reg8 RegA (frozenRegisters frozenCPU) ~?= 0x0
                                  , "z flag" ~: flagZ (frozenFlags frozenCPU) ~?= True
                                  , "h flag" ~: flagH (frozenFlags frozenCPU) ~?= True
                                  ] where
  frozenCPU = runST $ do
    let regs = setReg8 RegA 0x1 (setReg8 RegB 0x10 initRegisters)
    cpu <- withRegisters regs emptyCPU
    cpu' <- execStateT (and RegisterA (Reg8 RegB)) cpu
    freezeCPU cpu'

andRAHL :: Test
andRAHL = "AND A, [HL]" ~: TestList [ "register updated" ~: reg8 RegA (frozenRegisters frozenCPU) ~?= 0x0
                                    , "z flag" ~: flagZ (frozenFlags frozenCPU) ~?= True
                                    , "h flag" ~: flagH (frozenFlags frozenCPU) ~?= True
                                    ] where
  frozenCPU = runST $ do
    let regs = setReg8 RegA 0x1 (setReg16 RegH RegL 0x1111 initRegisters)
    cpu <- withRegisters regs emptyCPU
    writeRAM (ram cpu) 0x1111 0x10
    cpu' <- execStateT (and RegisterA IndirectHL) cpu
    freezeCPU cpu'

andRAN8 :: Test
andRAN8 = "AND A, n8" ~: TestList [ "register updated" ~: reg8 RegA (frozenRegisters frozenCPU) ~?= 0x0
                                  , "z flag" ~: flagZ (frozenFlags frozenCPU) ~?= True
                                  , "h flag" ~: flagH (frozenFlags frozenCPU) ~?= True
                                  ] where
  frozenCPU = runST $ do
    let regs = setReg8 RegA 0x1 initRegisters
    cpu <- withRegisters regs emptyCPU
    cpu' <- execStateT (and RegisterA (Uimm8 0x10)) cpu
    freezeCPU cpu'

andTests :: Test
andTests = "AND tests" ~: TestList [ andRAR8
                                   , andRAHL
                                   , andRAN8
                                   ]

{- TODO: add tests for CP -}
cpTests :: Test
cpTests = "CP tests" ~: TestList []

{- TODO: add test for HALT -}
haltTests :: Test
haltTests = "HALT tests" ~: TestList []

{- TODO: add test for NOP -}
nopTests :: Test
nopTests = "NOP tests" ~: TestList []

qc :: IO ()
qc = do
  quickCheck prop_load

unitTests :: Test
unitTests = "cpuTests" ~: TestList [ loadTests
                                   , addTests
                                   , andTests
                                   , cpTests
                                   , haltTests
                                   , nopTests
                                   ]

cpuTests :: IO ()
cpuTests = do
  _ <- runTestTT unitTests
  qc
