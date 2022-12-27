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
  , and
  )
where

import Control.Monad.State.Lazy
import Data.Array
import Data.Array.MArray
import Data.Bits
import Data.Word
import Prelude hiding (and)

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

step :: MArray a Word8 m => StateT (CPU a m) m (CPU a m)
step = do
  cpu <- get
  Ins instruction <- fetchInstruction
  executeInstruction instruction
  cpu' <- get
  return $ cpu' { pc = pc cpu + 1 } -- TODO: update PC correctly

load :: (MArray a Word8 m, LoadOperands k1 k2 ~ 'KLoad) => Operand k1 -> Operand k2 -> StateT (CPU a m) m ()
load (Reg8 r1) (Reg8 r2) = do
  cpu <- get
  let regs = registers cpu
  put $ cpu { registers = setReg8 r1 (reg8 r2 regs) regs }
load (Reg8 r1) (Uimm8 n8) =
  modify (\cpu -> cpu { registers = setReg8 r1 n8 (registers cpu) })
load (Reg16 r1 r2) (Uimm16 n16) =
  modify (\cpu -> cpu { registers = setReg16 r1 r2 n16 (registers cpu) })
load (Indirect (Reg16 RegH RegL)) (Reg8 r) = do
  cpu <- get
  let regs = registers cpu
  lift $ writeArray (ram cpu) (reg16 RegH RegL regs) (reg8 r regs)
load (Indirect (Reg16 RegH RegL)) (Uimm8 uimm8) = do
  cpu <- get
  let regs = registers cpu
  lift $ writeArray (ram cpu) (reg16 RegH RegL regs) uimm8
load (Reg8 r) (Indirect (Reg16 RegH RegL)) = do
  cpu <- get
  let regs = registers cpu
  n8 <- lift $ readArray (ram cpu) (reg16 RegH RegL regs)
  put $ cpu { registers = setReg8 r n8 regs }
load (Indirect (Reg16 r1 r2)) (Reg8 RegA) = do
  cpu <- get
  let regs = registers cpu
  lift $ writeArray (ram cpu) (reg16 r1 r2 regs) (reg8 RegA regs)
load (Indirect (Uimm16 n16)) (Reg8 RegA) = do
  cpu <- get
  let regs = registers cpu
  lift $ writeArray (ram cpu) n16 (reg8 RegA regs)
load (Indirect (FF00Offset op)) (Reg8 RegA) = do
  cpu <- get
  let regs = registers cpu
  lift $ writeArray (ram cpu) (offsetFF00 op regs) (reg8 RegA regs)
load (Reg8 RegA) (Indirect (Reg16 r1 r2)) = do
  cpu <- get
  let regs = registers cpu
  n8 <- lift $ readArray (ram cpu) (reg16 r1 r2 regs)
  put $ cpu { registers = setReg8 RegA n8 regs }
load (Reg8 RegA) (Indirect (Uimm16 n16)) = do
  cpu <- get
  let regs = registers cpu
  n8 <- lift $ readArray (ram cpu) n16
  put $ cpu { registers = setReg8 RegA n8 regs }
load (Reg8 RegA) (Indirect (FF00Offset op)) = do
  cpu <- get
  let regs = registers cpu
  n8 <- lift $ readArray (ram cpu) (offsetFF00 op regs)
  put $ cpu { registers = setReg8 RegA n8 regs }
load (Indirect postIns@(PostInstruction (Reg16 RegH RegL) _)) (Reg8 RegA) = do
  cpu <- get
  let regs = registers cpu
  lift $ writeArray (ram cpu) (reg16 RegH RegL regs) (reg8 RegA regs)
  put $ cpu { registers = postInstruction postIns regs }
load (Reg8 RegA) (Indirect postIns@(PostInstruction (Reg16 RegH RegL) _)) = do
  cpu <- get
  let regs = registers cpu
  n8 <- lift $ readArray (ram cpu) (reg16 RegH RegL regs)
  put $ cpu { registers = postInstruction postIns (setReg8 RegA n8 regs) }
load (StackPointer Unchanged) (Uimm16 n16) = modify (\cpu -> cpu { sp = n16 })
load (Indirect (Uimm16 n16)) (StackPointer Unchanged) = do
  cpu <- get
  lift $ writeArray (ram cpu) n16 (fromIntegral $ sp cpu .&. 0x00FF)
  lift $ writeArray (ram cpu) (n16 + 1) (fromIntegral $ shiftR (sp cpu) 8)
load (Reg16 RegH RegL) (StackPointer spo@(AddInt8 e8)) = do
  cpu <- get
  let flags' = flagsFromInt8 (fromIntegral e8 + fromIntegral (sp cpu))
  let regs' = setReg16 RegH RegL (offsetSP spo (sp cpu)) (registers cpu)
  put $ cpu { registers = regs', flags = flags' { flagZ = False } }
load (StackPointer Unchanged) (Reg16 RegH RegL) =
  modify (\cpu -> cpu { sp = reg16 RegH RegL (registers cpu) })

evalOpWord8 :: MArray a Word8 m => Operand k -> StateT (CPU a m) m Word8
evalOpWord8 (Uimm8 n8) = return n8
evalOpWord8 (Reg8 r) = reg8 r . registers <$> get
evalOpWord8 (Indirect (Reg16 RegH RegL)) = do
  cpu <- get
  let regs = registers cpu
  lift $ readArray (ram cpu) (reg16 RegH RegL regs)
evalOpWord8 _ = undefined -- TODO: add additional possible cases

evalOpWord16 :: MArray a Word8 m => Operand k -> StateT (CPU a m) m Word16
evalOpWord16 (Reg16 r1 r2) = reg16 r1 r2 . registers <$> get
evalOpWord16 (StackPointer Unchanged) = sp <$> get
evalOpWord16 _ = undefined -- TODO: add additional possible cases

add :: (MArray a Word8 m, AddOperands atk k1 k2 ~ 'KAdd) => ArithmeticType atk -> Operand k1 -> Operand k2 -> StateT (CPU a m) m ()
add at (Reg8 RegA) op = do
  cpu <- get
  let regs = registers cpu
  opVal <- evalOpWord8 op
  let aVal = reg8 RegA regs
  let added = fromIntegral aVal + fromIntegral opVal
  let flags' = flagsFromInt8 added
  let carryVal = case at of
                   WithCarryIncluded -> if flagC flags' then 1 else 0
                   WithoutCarryIncluded -> 0
  put $ cpu { registers = setReg8 RegA (aVal + opVal + carryVal) regs, flags = flags' }
add WithoutCarryIncluded (Reg16 RegH RegL) op = do
  cpu <- get
  let regs = registers cpu
  let hlVal = reg16 RegH RegL regs
  opVal <- evalOpWord16 op
  let flags' = flagsFromInt16 (fromIntegral hlVal + fromIntegral opVal)
  let regs' = setReg16 RegH RegL (hlVal + opVal) regs
  put $ cpu { registers = regs', flags = flags' { flagZ = False } }
add WithoutCarryIncluded (StackPointer Unchanged) (Imm8 e8) = do
  cpu <- get
  let flags' = flagsFromInt8 (fromIntegral e8 + fromIntegral (sp cpu))
  put $ cpu { sp = offsetSP (AddInt8 e8) (sp cpu), flags = flags' }

and :: forall a m k1 k2. (MArray a Word8 m, AndOperands k1 k2 ~ 'KAnd) => Operand k1 -> Operand k2 -> StateT (CPU a m) m ()
and (Reg8 RegA) op = do
  cpu <- get
  let regs = registers cpu
  opVal <- evalOpWord8 op
  let regs' = setReg8 RegA (reg8 RegA regs .&. opVal) regs
  let z = reg8 RegA regs' == 0
  put $ cpu { registers = regs', flags = emptyFlags { flagZ = z, flagH = True } }

-- TODO: implement
fetchInstruction :: Monad m => StateT (CPU a m) m Ins
fetchInstruction = do
  cpu <- get
  case pc cpu of
    0 ->
      let r = if True then RegB else RegC in
      return $ Ins (Add WithCarryIncluded (Reg8 RegA) (Reg8 r))
    _ -> return $ Ins (Add WithCarryIncluded (Reg8 RegA) (Uimm8 10))

executeInstruction :: MArray a Word8 m => Instruction k -> StateT (CPU a m) m ()
executeInstruction (Load o1 o2) = load o1 o2
executeInstruction (Add at o1 o2) = add at o1 o2
executeInstruction (And o1 o2) = and o1 o2
executeInstruction Halt = modify (\cpu -> cpu { running = False })
executeInstruction Nop = return ()
executeInstruction _ = undefined
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
