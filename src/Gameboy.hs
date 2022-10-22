module Gameboy
    ( run
    )
where

import Data.Array.MArray
import Data.Word

import Instruction
import Operand
import Registers

data CPU a m where
  CPU :: MArray a Word8 m => {
    pc :: Word16,
    sp :: Word16,
    ram :: a Word16 Word8,
    registers :: Registers
  } -> CPU a m

initCPU :: (Monad m, MArray a Word8 m) => m (CPU a m)
initCPU = do
  arr <- newArray_ (0x0000, 0xFFFF)
  return $ CPU {
    pc = 0,
    sp = 0,
    ram = arr,
    registers = emptyRegisters
  }

load :: forall m k1 k2 a. (Monad m, MArray a Word8 m, LoadOperands k1 k2 ~ 'KLoad) => Operand k1 -> Operand k2 -> (CPU a m -> m (CPU a m))
load (Reg8 r1) (Reg8 r2) cpu = do
  let regs = registers cpu
  return $ cpu { registers = setReg8 r1 (reg8 r2 regs) regs }
load (Reg8 r1) (Uimm8 n8) cpu = do
  let regs = registers cpu
  return $ cpu { registers = setReg8 r1 n8 regs }
load (Reg16 r1 r2) (Uimm16 n16) cpu = do
  let regs = registers cpu
  return $ cpu { registers = setReg16 r1 r2 n16 regs }
load (Indirect (Reg16 RegH RegL)) (Reg8 r) cpu = do
  let regs = registers cpu
  _ <- writeArray (ram cpu) (reg16 RegH RegL regs) (reg8 r regs)
  return cpu
load (Indirect (Reg16 RegH RegL)) (Uimm8 uimm8) cpu = do
  let regs = registers cpu
  _ <- writeArray (ram cpu) (reg16 RegH RegL regs) uimm8
  return cpu
load (Reg8 r) (Indirect (Reg16 RegH RegL)) cpu = do
  let regs = registers cpu
  n8 <- readArray (ram cpu) (reg16 RegH RegL regs)
  return $ cpu { registers = setReg8 r n8 regs }
load (Indirect (Reg16 r1 r2)) (Reg8 RegA) cpu = do
  let regs = registers cpu
  _ <- writeArray (ram cpu) (reg16 r1 r2 regs) (reg8 RegA regs)
  return cpu
load (Indirect (Uimm16 n16)) (Reg8 RegA) cpu = do
  let regs = registers cpu
  _ <- writeArray (ram cpu) n16 (reg8 RegA regs)
  return cpu
load (Indirect (FF00Offset op)) (Reg8 RegA) cpu = do
  let regs = registers cpu
  _ <- writeArray (ram cpu) (offsetFF00 op regs) (reg8 RegA regs)
  return cpu
load (Reg8 RegA) (Indirect (Reg16 r1 r2)) cpu = do
  let regs = registers cpu
  n8 <- readArray (ram cpu) (reg16 r1 r2 regs)
  return $ cpu { registers = setReg8 RegA n8 regs }
load (Reg8 RegA) (Indirect (Uimm16 n16)) cpu = do
  let regs = registers cpu
  n8 <- readArray (ram cpu) n16
  return $ cpu { registers = setReg8 RegA n8 regs }
load (Reg8 RegA) (Indirect (FF00Offset op)) cpu = do
  let regs = registers cpu
  n8 <- readArray (ram cpu) (offsetFF00 op regs)
  return $ cpu { registers = setReg8 RegA n8 regs }
load (Indirect (PostInstruction (Reg16 RegH RegL) operation)) (Reg8 RegA) cpu = undefined
load (Reg8 RegA) (Indirect (PostInstruction (Reg16 RegH RegL) operation)) cpu = undefined
load _ _ _ = undefined

executeInstruction :: forall a m k. (Monad m, MArray a Word8 m) => Instruction k -> CPU a m -> m (CPU a m)
executeInstruction (Load (o1 :: Operand k1) (o2 :: Operand k2)) cpu = load o1 o2 cpu
executeInstruction _ _ = undefined
{- TODO: implement each of these
executeInstruction ins@Add {} = addIns ins where
  addIns :: Instruction 'KAdd -> IO ()
  addIns (Add WithCarryIncluded _ _) = undefined
  addIns (Add WithoutCarryIncluded _ _) = undefined
executeInstruction ins@And {} = andIns ins where
  andIns :: Instruction 'KAnd -> IO ()
  andIns _ = undefined
executeInstruction ins@Compare {} = compareIns ins where
  compareIns :: Instruction 'KCompare -> IO ()
  compareIns _ = undefined
executeInstruction ins@(Decrement _) = decrementIns ins where
  decrementIns :: Instruction 'KDecrement -> IO ()
  decrementIns = undefined
executeInstruction ins@(Increment _) = incrementIns ins where
  incrementIns :: Instruction 'KIncrement -> IO ()
  incrementIns _ = undefined
executeInstruction ins@Or {} = orIns ins where
  orIns :: Instruction 'KOr -> IO ()
  orIns = undefined
executeInstruction ins@Sub {} = subIns ins where
  subIns :: Instruction 'KSub -> IO ()
  subIns = undefined
executeInstruction ins@Xor {} = xorIns ins where
  xorIns :: Instruction 'KXor -> IO ()
  xorIns _ = undefined
executeInstruction ins@Bit {} = bitIns ins where
  bitIns :: Instruction 'KBit -> IO ()
  bitIns (Bit Uimm3 _) = undefined
executeInstruction ins@Res {} = resIns ins where
  resIns :: Instruction 'KRes -> IO ()
  resIns _ = undefined
executeInstruction ins@Set {} = setIns ins where
  setIns :: Instruction 'KSet -> IO ()
  setIns _ = undefined
executeInstruction ins@(Swap _) = swapIns ins where
  swapIns :: Instruction 'KSwap -> IO ()
  swapIns _ = undefined
executeInstruction ins@Rotate {} = rotateIns ins where
  rotateIns :: Instruction 'KRotate -> IO ()
  rotateIns (Rotate RotateRight DefaultRotate _) = undefined
  rotateIns (Rotate RotateLeft ThroughCarry _) = undefined
  rotateIns _ = undefined
executeInstruction ins@Shift {} = shiftIns ins where
  shiftIns :: Instruction 'KShift -> IO ()
  shiftIns (Shift ShiftRight ShiftLogically _ _) = undefined
  shiftIns (Shift ShiftLeft ShiftArithmetically _ _) = undefined
  shiftIns _ = undefined
executeInstruction ins@Call {} = callIns ins where
  callIns :: Instruction 'KCall -> IO ()
  callIns (Call CodeZ _) = undefined
  callIns (Call CodeNZ _) = undefined
  callIns (Call CodeC _) = undefined
  callIns (Call CodeNC _) = undefined
  callIns (Call NegateCode _) = undefined
  callIns (Call EmptyCode _) = undefined
executeInstruction ins@Jump {} = jumpIns ins where
  jumpIns :: Instruction 'KJump -> IO ()
  jumpIns _ = undefined
executeInstruction ins@Ret {} = retIns ins where
  retIns :: Instruction 'KRet -> IO ()
  retIns (Ret _ PostRetNoop) = undefined
  retIns (Ret _ PostRetEnableInterrupts) = undefined
executeInstruction ins@Rst {} = rstIns ins where
  rstIns :: Instruction 'KRst -> IO ()
  rstIns (Rst RstVector) = undefined
executeInstruction ins@Pop {} = popIns ins where
  popIns :: Instruction 'KPop -> IO ()
  popIns _ = undefined
executeInstruction ins@Push {} = pushIns ins where
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
executeInstruction ins@ToggleInterrupts {} = toggleInterruptsIns ins where
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
-}

-- TODO: actually load a cart
run :: forall m a. (Monad m, MArray a Word8 m) => m ()
run = do
  cpu <- initCPU @m @a
  _ <- executeInstruction Halt cpu
  return ()
