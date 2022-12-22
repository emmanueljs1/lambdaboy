module CPU
  ( CPU (..)
  , Flags (..)
  , ResultCPU (..)
  , initCPU
  , toResultCPU
  , step
  , fetchInstruction
  , executeInstruction
  , load
  , add
  )
where

import Prelude hiding (and)

import Data.Array
import Data.Array.MArray
import Data.Bits
import Data.Word

import Instruction
import Operand
import Registers

data Flags = Flags { flagZ :: Bool, flagN :: Bool, flagH :: Bool, flagC :: Bool }

emptyFlags :: Flags
emptyFlags = Flags { flagZ = False, flagN = False, flagH = False, flagC = False }

flagsFromInt8 :: Int -> Flags
flagsFromInt8 i =
  let h = i > 0xF in
  let z = i `mod` 0x100 == 0 in
  let c = i > 0xFF in
  emptyFlags { flagH = h, flagZ = z, flagC = c }

flagsFromInt16 :: Int -> Flags
flagsFromInt16 i =
  let h = i > 0xFF in
  let z = i `mod` 0x10000 == 0 in
  let c = i > 0xFFFF in
  emptyFlags { flagH = h, flagZ = z, flagC = c }

data CPU a m where
  CPU :: MArray a Word8 m => { pc :: Word16
                             , sp :: Word16
                             , ram :: a Word16 Word8
                             , registers :: Registers
                             , flags :: Flags
                             , running :: Bool
                             } -> CPU a m

initCPU :: MArray a Word8 m => m (CPU a m)
initCPU = do
  arr <- newArray_ (0x0000, 0xFFFF)
  return $ CPU { pc = 0
               , sp = 0
               , ram = arr
               , registers = emptyRegisters
               , flags = emptyFlags
               , running = True
               }

data ResultCPU =  ResultCPU { resultPC :: Word16
                            , resultSP :: Word16
                            , resultRAM :: Array Word16 Word8
                            , resultRegisters :: Registers
                            , resultFlags :: Flags
                            , resultRunning :: Bool
                            }

toResultCPU :: MArray a Word8 m => CPU a m -> m ResultCPU
toResultCPU cpu = do
  frozenRAM <- freeze $ ram cpu
  return $ ResultCPU { resultPC = pc cpu
                     , resultSP = sp cpu
                     , resultRAM = frozenRAM
                     , resultRegisters = registers cpu
                     , resultFlags = flags cpu
                     , resultRunning = running cpu
                     }

step :: MArray a Word8 m => CPU a m -> m (CPU a m)
step cpu = do
  Ins instruction <- fetchInstruction cpu
  cpu' <- executeInstruction instruction cpu
  return $ cpu' { pc = pc cpu + 1 } -- TODO: update PC correctly


load :: (MArray a Word8 m, LoadOperands k1 k2 ~ 'KLoad) => Operand k1 -> Operand k2 -> CPU a m -> m (CPU a m)
load (Reg8 r1) (Reg8 r2) cpu =
  let regs = registers cpu in
  return $ cpu { registers = setReg8 r1 (reg8 r2 regs) regs }
load (Reg8 r1) (Uimm8 n8) cpu =
  let regs = registers cpu in
  return $ cpu { registers = setReg8 r1 n8 regs }
load (Reg16 r1 r2) (Uimm16 n16) cpu =
  let regs = registers cpu in
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
load (Indirect postIns@(PostInstruction (Reg16 RegH RegL) _)) (Reg8 RegA) cpu = do
  let regs = registers cpu
  _ <- writeArray (ram cpu) (reg16 RegH RegL regs) (reg8 RegA regs)
  return $ cpu { registers = postInstruction postIns regs }
load (Reg8 RegA) (Indirect postIns@(PostInstruction (Reg16 RegH RegL) _)) cpu = do
  let regs = registers cpu
  n8 <- readArray (ram cpu) (reg16 RegH RegL regs)
  return $ cpu { registers = postInstruction postIns (setReg8 RegA n8 regs) }
load (StackPointer Unchanged) (Uimm16 n16) cpu =
  return $ cpu { sp = n16 }
load (Indirect (Uimm16 n16)) (StackPointer Unchanged) cpu = do
  _ <- writeArray (ram cpu) n16 (fromIntegral $ sp cpu .&. 0x00FF)
  _ <- writeArray (ram cpu) (n16 + 1) (fromIntegral $ shiftR (sp cpu) 8)
  return cpu
load (Reg16 RegH RegL) (StackPointer spo@(AddInt8 e8)) cpu =
  let flags' = flagsFromInt8 (fromIntegral e8 + fromIntegral (sp cpu)) in
  let regs' = setReg16 RegH RegL (offsetSP spo (sp cpu)) (registers cpu) in
  return $ cpu { registers = regs', flags = flags' { flagZ = False } }
load (StackPointer Unchanged) (Reg16 RegH RegL) cpu =
  let regs = registers cpu in
  return $ cpu { sp = reg16 RegH RegL regs }

add :: forall a m atk k1 k2. (MArray a Word8 m, AddOperands atk k1 k2 ~ 'KAdd) => ArithmeticType atk -> Operand k1 -> Operand k2 -> CPU a m -> m (CPU a m)
add at (Reg8 RegA) op cpu = do
  opVal <- evalOp op
  let added = fromIntegral aVal + fromIntegral opVal
  let flags' = flagsFromInt8 added
  let carryVal = case at of
                   WithCarryIncluded -> if flagC flags' then 1 else 0
                   WithoutCarryIncluded -> 0
  return $ cpu { registers = setReg8 RegA (aVal + opVal + carryVal) regs, flags = flags' }
  where
    regs = registers cpu
    evalOp :: AddOperands atk ('KReg8 'A) k2 ~ 'KAdd => Operand k2 -> m Word8
    evalOp (Reg8 r) = return $ reg8 r regs
    evalOp (Indirect (Reg16 RegH RegL)) = readArray (ram cpu) (reg16 RegH RegL regs)
    evalOp (Uimm8 n8) = return n8
    aVal = reg8 RegA regs
add WithoutCarryIncluded (Reg16 RegH RegL) op cpu =
  return $ cpu { registers = regs', flags = flags' { flagZ = False } }
  where
    regs = registers cpu
    evalOp :: AddOperands atk ('KReg16 'H 'L) k2 ~ 'KAdd => Operand k2 -> Word16
    evalOp (Reg16 r1 r2) = reg16 r1 r2 regs
    evalOp (StackPointer Unchanged) = sp cpu
    hlVal = reg16 RegH RegL regs
    opVal = evalOp op
    flags' = flagsFromInt16 (fromIntegral hlVal + fromIntegral opVal)
    regs' = setReg16 RegH RegL (hlVal + opVal) regs
add WithoutCarryIncluded (StackPointer Unchanged) (Imm8 e8) cpu =
  let flags' = flagsFromInt8 (fromIntegral e8 + fromIntegral (sp cpu)) in
  return $ cpu { sp = offsetSP (AddInt8 e8) (sp cpu), flags = flags' }

and :: forall a m k1 k2. (MArray a Word8 m, AndOperands k1 k2 ~ 'KAnd) => Operand k1 -> Operand k2 -> CPU a m -> m (CPU a m)
and (Reg8 RegA) op cpu = do
  opVal <- evalOp op
  let z = (reg8 RegA regs .&. opVal) == 0
  return $ cpu { flags = emptyFlags { flagZ = z, flagH = True } }
  where
    regs = registers cpu
    -- TODO: generalize this to evalArithmeticOp (for add, and, sbc, sub, or, xor)
    evalOp :: (MArray a Word8 m, AndOperands k1 k2 ~ 'KAnd) => Operand k2 -> m Word8
    evalOp (Reg8 r) = return $ reg8 r regs
    evalOp (Indirect (Reg16 RegH RegL)) = readArray (ram cpu) (reg16 RegH RegL regs)
    evalOp (Uimm8 n8) = return n8

-- TODO: implement
fetchInstruction :: Monad m => CPU a m -> m Ins
fetchInstruction _ = undefined

executeInstruction :: MArray a Word8 m => Instruction k -> CPU a m -> m (CPU a m)
executeInstruction (Load o1 o2) cpu = load o1 o2 cpu
executeInstruction (Add at o1 o2) cpu = add at o1 o2 cpu
executeInstruction (And o1 o2) cpu = and o1 o2 cpu
executeInstruction Halt cpu = return $ cpu { running = False }
executeInstruction Nop cpu = return cpu
executeInstruction _ _ = undefined
{- TODO: implement each of these
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
executeInstruction ins@SetCarryFlag = scfIns ins where
  scfIns :: Instruction 'KSetCarryFlag -> IO ()
  scfIns _ = undefined
executeInstruction ins@Stop = stopIns ins where
  stopIns :: Instruction 'KStop -> IO ()
  stopIns _ = undefined
-}
