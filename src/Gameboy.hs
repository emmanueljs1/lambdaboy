module Gameboy
    ( run
    )
where

import Operand
import Instruction

-- TODO: implement
executeInstruction :: Instruction k -> IO ()
executeInstruction ins@Load {} = loadIns ins where
  loadIns :: Instruction 'KLoad -> IO ()
  loadIns _ = undefined
executeInstruction ins@Add {} = addIns ins where
  addIns :: Instruction 'KAdd -> IO ()
  addIns (Add WithCarryIncluded _ _) = undefined
  addIns (Add WithoutCarryIncluded _ _) = undefined
executeInstruction ins@(And {}) = andIns ins where
  andIns :: Instruction 'KAnd -> IO ()
  andIns _ = undefined
executeInstruction ins@(Compare {}) = compareIns ins where
  compareIns :: Instruction 'KCompare -> IO ()
  compareIns _ = undefined
executeInstruction ins@(Decrement _) = decrementIns ins where
  decrementIns :: Instruction 'KDecrement -> IO ()
  decrementIns = undefined
executeInstruction ins@(Increment _) = incrementIns ins where
  incrementIns :: Instruction 'KIncrement -> IO ()
  incrementIns _ = undefined
executeInstruction ins@(Or {}) = orIns ins where
  orIns :: Instruction 'KOr -> IO ()
  orIns = undefined
executeInstruction ins@(Sub {}) = subIns ins where
  subIns :: Instruction 'KSub -> IO ()
  subIns = undefined
executeInstruction ins@(Xor {}) = xorIns ins where
  xorIns :: Instruction 'KXor -> IO ()
  xorIns _ = undefined
executeInstruction ins@(Bit {}) = bitIns ins where
  bitIns :: Instruction 'KBit -> IO ()
  bitIns (Bit Uimm3 _) = undefined
executeInstruction ins@(Res {}) = resIns ins where
  resIns :: Instruction 'KRes -> IO ()
  resIns _ = undefined
executeInstruction ins@(Set {}) = setIns ins where
  setIns :: Instruction 'KSet -> IO ()
  setIns _ = undefined
executeInstruction ins@(Swap _) = swapIns ins where
  swapIns :: Instruction 'KSwap -> IO ()
  swapIns _ = undefined
executeInstruction ins@(Rotate {}) = rotateIns ins where
  rotateIns :: Instruction 'KRotate -> IO ()
  rotateIns (Rotate RotateRight DefaultRotate _) = undefined
  rotateIns (Rotate RotateLeft ThroughCarry _) = undefined
  rotateIns _ = undefined
executeInstruction ins@(Shift {}) = shiftIns ins where
  shiftIns :: Instruction 'KShift -> IO ()
  shiftIns (Shift ShiftRight ShiftLogically _ _) = undefined
  shiftIns (Shift ShiftLeft ShiftArithmetically _ _) = undefined
  shiftIns _ = undefined
executeInstruction ins@(Call {}) = callIns ins where
  callIns :: Instruction 'KCall -> IO ()
  callIns (Call CodeZ _) = undefined
  callIns (Call CodeNZ _) = undefined
  callIns (Call CodeC _) = undefined
  callIns (Call CodeNC _) = undefined
  callIns (Call NegateCode _) = undefined
  callIns (Call EmptyCode _) = undefined
executeInstruction ins@(Jump {}) = jumpIns ins where
  jumpIns :: Instruction 'KJump -> IO ()
  jumpIns _ = undefined
executeInstruction ins@(Ret {}) = retIns ins where
  retIns :: Instruction 'KRet -> IO ()
  retIns (Ret _ PostRetNoop) = undefined
  retIns (Ret _ PostRetEnableInterrupts) = undefined
executeInstruction ins@(Rst {}) = rstIns ins where
  rstIns :: Instruction 'KRst -> IO ()
  rstIns (Rst RstVector) = undefined
executeInstruction ins@(Pop {}) = popIns ins where
  popIns :: Instruction 'KPop -> IO ()
  popIns _ = undefined
executeInstruction ins@(Push {}) = pushIns ins where
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
executeInstruction ins@(ToggleInterrupts {}) = toggleInterruptsIns ins where
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

run :: IO ()
run = executeInstruction Halt
