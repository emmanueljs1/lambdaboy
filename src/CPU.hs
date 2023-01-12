module CPU
  ( CPU (..)
  , Flags (..)
  , FrozenCPU (..)
  , initCPU
  , freezeCPU
  , fromFrozenCPU
  , step
  , fetchInstruction
  , executeInstruction
  , load
  , add
  , and
  , writeRAM
  , evalOp
  , readFrozenRAM
  )
where

import Control.Monad.State.Lazy
import Data.Array
import Data.Array.MArray
import Data.Bits
import Data.Foldable (toList)
import Data.List (intersperse)
import Data.Word
import Prelude hiding (and)
import Test.QuickCheck (Arbitrary, arbitrary, vectorOf)

import Instruction
import Operand
import Registers

data Flags = Flags { flagZ :: Bool, flagN :: Bool, flagH :: Bool, flagC :: Bool }

instance Show Flags where
  show (Flags z n h c) = "{" ++ join listed ++ "}" where
    listed = intersperse ", " $ map (\(x,y) -> x ++ "=" ++ show y) lst
    lst = zip ["z","n","h","c"] [z,n,h,c]

instance Arbitrary Flags where
  arbitrary = Flags <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

initFlags :: Flags
initFlags = Flags { flagZ = False, flagN = False, flagH = False, flagC = False }

flagsFromInt8 :: Int -> Flags
flagsFromInt8 i =
  let h = i > 0xF in
  let z = i `mod` 0x100 == 0 in
  let c = i > 0xFF in
  initFlags { flagH = h, flagZ = z, flagC = c }

flagsFromInt16 :: Int -> Flags
flagsFromInt16 i =
  let h = i > 0xFF in
  let z = i `mod` 0x10000 == 0 in
  let c = i > 0xFFFF in
  initFlags { flagH = h, flagZ = z, flagC = c }

data RAM a = RAM (a Word16 Word8) (a Bool Word8)

initRAM :: MArray a Word8 m => m (RAM a)
initRAM = do
  arr <- newArray_ (minBound, maxBound)
  ffff <- newArray_ (minBound, maxBound)
  return $ RAM arr ffff

fromFrozenRAM :: MArray a Word8 m => FrozenRAM -> m (RAM a)
fromFrozenRAM (FrozenRAM arr n8) = do
  a <- newListArray (minBound, maxBound) $ toList arr
  b <- newArray (minBound, maxBound) n8
  return $ RAM a b

readRAM :: MArray a Word8 m => RAM a -> Word16 -> m Word8
readRAM (RAM _ b) 0xFFFF = readArray b False
readRAM (RAM a _) i = readArray a i

writeRAM :: MArray a Word8 m => RAM a -> Word16 -> Word8 -> m ()
writeRAM (RAM _ b) 0xFFFF n8 = writeArray b False n8
writeRAM (RAM a _) i n8 = writeArray a i n8

data CPU a m where
  CPU :: MArray a Word8 m => { pc :: Word16
                             , sp :: Word16
                             , ram :: RAM a
                             , registers :: Registers
                             , flags :: Flags
                             , running :: Bool
                             } -> CPU a m

initCPU :: MArray a Word8 m => m (CPU a m)
initCPU = do
  initRam <- initRAM
  return $ CPU { pc = 0
               , sp = 0
               , ram = initRam
               , registers = initRegisters
               , flags = initFlags
               , running = True
               }

data FrozenRAM = FrozenRAM (Array Word16 Word8) Word8

instance Show FrozenRAM where
  show _ = "(...)"

instance Arbitrary FrozenRAM where
  arbitrary = FrozenRAM <$> arr <*> arbitrary where
    arr = listArray (minBound, maxBound) <$> vectorOf 0x10000 arbitrary

fromRAM :: MArray a Word8 m => RAM a -> m FrozenRAM
fromRAM (RAM a b) = do
  frozenArr <- freeze a
  n8 <- readArray b False
  return $ FrozenRAM frozenArr n8

readFrozenRAM :: FrozenRAM -> Word16 -> Word8
readFrozenRAM (FrozenRAM _ n8) 0xFFFF = n8
readFrozenRAM (FrozenRAM arr _) n16 = arr ! n16

data FrozenCPU =  FrozenCPU { frozenPC :: Word16
                            , frozenSP :: Word16
                            , frozenRAM :: FrozenRAM
                            , frozenRegisters :: Registers
                            , frozenFlags :: Flags
                            , frozenRunning :: Bool
                            } deriving Show 

instance Arbitrary FrozenCPU where
  -- TODO: make PC / SP less arbitrary
  arbitrary = FrozenCPU <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> pure True

fromFrozenCPU :: forall a m. MArray a Word8 m => FrozenCPU -> m (CPU a m)
fromFrozenCPU frozenCPU = do
  thawedRam <- fromFrozenRAM $ frozenRAM frozenCPU
  return $ CPU { pc = frozenPC frozenCPU
               , sp = frozenSP frozenCPU
               , ram = thawedRam
               , registers = frozenRegisters frozenCPU
               , flags = frozenFlags frozenCPU
               , running = frozenRunning frozenCPU
               }

freezeCPU :: MArray a Word8 m => CPU a m -> m FrozenCPU
freezeCPU cpu = do
  frozenRam <- fromRAM $ ram cpu
  return $ FrozenCPU { frozenPC = pc cpu
                     , frozenSP = sp cpu
                     , frozenRAM = frozenRam
                     , frozenRegisters = registers cpu
                     , frozenFlags = flags cpu
                     , frozenRunning = running cpu
                     }

evalOp :: FrozenCPU -> Operand ok -> Bool -> Int
evalOp _ (Uimm8 n8) _ = fromIntegral n8
evalOp _ (Imm8 i8) _ = fromIntegral i8
evalOp cpu (Reg8 r) _ = fromIntegral $ reg8 r (frozenRegisters cpu)
evalOp _ (Uimm16 n16) _ = fromIntegral n16
evalOp cpu (Reg16 r1 r2) _ = fromIntegral $ reg16 r1 r2 (frozenRegisters cpu)
evalOp cpu (Indirect op) undo =
  let addr = evalOp cpu op undo in
  fromIntegral $ readFrozenRAM (frozenRAM cpu) (fromIntegral addr)
evalOp cpu (FF00Offset op) _ = fromIntegral $ offsetFF00 op (frozenRegisters cpu)
evalOp cpu (StackPointer Unchanged) _ = fromIntegral $ frozenSP cpu
evalOp cpu (StackPointer (AddInt8 i8)) undo = fromIntegral i8 + evalOp cpu (StackPointer Unchanged) undo
evalOp cpu RegisterHLI undo =
  let base = evalOp cpu RegisterHL undo in
  if undo then base - 1 else base
evalOp cpu RegisterHLD undo =
  let base = evalOp cpu RegisterHL undo in
  if undo then base + 1 else base
evalOp cpu RegisterHL undo = evalOp cpu (Reg16 RegH RegL) undo
evalOp cpu RegisterA undo = evalOp cpu (Reg8 RegA) undo
evalOp _ RegisterAF _ = undefined -- TODO
evalOp cpu RegisterC undo = evalOp cpu (Reg8 RegC) undo
evalOp cpu IndirectHL undo = evalOp cpu (Indirect (Reg16 RegH RegL)) undo
evalOp cpu (IndirectUimm16 n16) undo = evalOp cpu (Indirect (Uimm16 n16)) undo

step :: MArray a Word8 m => StateT (CPU a m) m (CPU a m)
step = do
  cpu <- get
  instruction <- fetchInstruction
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
load IndirectHL (Reg8 r) = do
  cpu <- get
  let regs = registers cpu
  lift $ writeRAM (ram cpu) (reg16 RegH RegL regs) (reg8 r regs)
load IndirectHL (Uimm8 uimm8) = do
  cpu <- get
  let regs = registers cpu
  lift $ writeRAM (ram cpu) (reg16 RegH RegL regs) uimm8
load (Reg8 r) IndirectHL = do
  cpu <- get
  let regs = registers cpu
  n8 <- lift $ readRAM (ram cpu) (reg16 RegH RegL regs)
  put $ cpu { registers = setReg8 r n8 regs }
load (Indirect RegisterHL) RegisterA = load (Indirect (Reg16 RegH RegL)) RegisterA
load (Indirect (Reg16 r1 r2)) RegisterA = do
  cpu <- get
  let regs = registers cpu
  lift $ writeRAM (ram cpu) (reg16 r1 r2 regs) (reg8 RegA regs)
load (Indirect (Uimm16 n16)) RegisterA = do
  cpu <- get
  let regs = registers cpu
  lift $ writeRAM (ram cpu) n16 (reg8 RegA regs)
load (Indirect (FF00Offset op)) RegisterA = do
  cpu <- get
  let regs = registers cpu
  lift $ writeRAM (ram cpu) (offsetFF00 op regs) (reg8 RegA regs)
load RegisterA (Indirect RegisterHL) = load RegisterA (Indirect (Reg16 RegH RegL))
load RegisterA (Indirect (Reg16 r1 r2)) = do
  cpu <- get
  let regs = registers cpu
  n8 <- lift $ readRAM (ram cpu) (reg16 r1 r2 regs)
  put $ cpu { registers = setReg8 RegA n8 regs }
load RegisterA (Indirect (Uimm16 n16)) = do
  cpu <- get
  let regs = registers cpu
  n8 <- lift $ readRAM (ram cpu) n16
  put $ cpu { registers = setReg8 RegA n8 regs }
load RegisterA (Indirect (FF00Offset op)) = do
  cpu <- get
  let regs = registers cpu
  n8 <- lift $ readRAM (ram cpu) (offsetFF00 op regs)
  put $ cpu { registers = setReg8 RegA n8 regs }
load (Indirect RegisterHLI) RegisterA = do
  cpu <- get
  let regs = registers cpu
  let regHL = reg16 RegH RegL regs
  lift $ writeRAM (ram cpu) regHL (reg8 RegA regs)
  let regs' = setReg16 RegH RegL (regHL + 1) regs -- TODO: use helper functions for I / D
  put $ cpu { registers = regs' }
load (Indirect RegisterHLD) RegisterA = do
  cpu <- get
  let regs = registers cpu
  let regHL = reg16 RegH RegL regs
  lift $ writeRAM (ram cpu) regHL (reg8 RegA regs)
  let regs' = setReg16 RegH RegL (regHL - 1) regs
  put $ cpu { registers = regs' }
load RegisterA (Indirect RegisterHLI) = do
  cpu <- get
  let regs = registers cpu
  let regHL = reg16 RegH RegL regs
  n8 <- lift $ readRAM (ram cpu) regHL
  let regs' = setReg16 RegH RegL (regHL + 1) regs
  put $ cpu { registers = setReg8 RegA n8 regs' }
load RegisterA (Indirect RegisterHLD) = do
  cpu <- get
  let regs = registers cpu
  let regHL = reg16 RegH RegL regs
  n8 <- lift $ readRAM (ram cpu) regHL
  let regs' = setReg16 RegH RegL (regHL - 1) regs
  put $ cpu { registers = setReg8 RegA n8 regs' }
load (StackPointer Unchanged) (Uimm16 n16) = modify (\cpu -> cpu { sp = n16 })
load (IndirectUimm16 n16) (StackPointer Unchanged) = do
  cpu <- get
  lift $ writeRAM (ram cpu) n16 (fromIntegral $ sp cpu .&. 0x00FF)
  lift $ writeRAM (ram cpu) (n16 + 1) (fromIntegral $ shiftR (sp cpu) 8)
load RegisterHL (StackPointer spo@(AddInt8 e8)) = do
  cpu <- get
  let flags' = flagsFromInt8 (fromIntegral e8 + fromIntegral (sp cpu))
  let regs' = setReg16 RegH RegL (offsetSP spo (sp cpu)) (registers cpu)
  put $ cpu { registers = regs', flags = flags' { flagZ = False } }
load (StackPointer Unchanged) RegisterHL =
  modify (\cpu -> cpu { sp = reg16 RegH RegL (registers cpu) })

class Word8Operand o where
  evalOp8 :: MArray a Word8 m => o -> StateT (CPU a m) m Word8

instance Word8Operand (Operand 'KUimm8) where
  evalOp8 (Uimm8 n8) = return n8

instance Word8Operand (Operand 'KReg8) where
  evalOp8 (Reg8 r) = reg8 r . registers <$> get

instance Word8Operand (Operand 'KIndirectHL) where
  evalOp8 _ = do
    cpu <- get
    let regs = registers cpu
    lift $ readRAM (ram cpu) (reg16 RegH RegL regs)

class Word16Operand o where
  evalOp16 :: MArray a Word8 m => o -> StateT (CPU a m) m Word16

instance Word16Operand (Operand 'KReg16) where
  evalOp16 (Reg16 r1 r2) = reg16 r1 r2 . registers <$> get

instance Word16Operand (Operand ('KStackPointer 'KUnchanged)) where
  evalOp16 _ = sp <$> get

add :: (MArray a Word8 m, AddOperands atk k1 k2 ~ 'KAdd) => ArithmeticType atk -> Operand k1 -> Operand k2 -> StateT (CPU a m) m ()
add at RegisterA op = do
  cpu <- get
  let regs = registers cpu
  opVal <- case op of
             Reg8 _ -> evalOp8 op
             Uimm8 _ -> evalOp8 op
             IndirectHL -> evalOp8 op
  let aVal = reg8 RegA regs
  let added = fromIntegral aVal + fromIntegral opVal
  let flags' = flagsFromInt8 added
  let carryVal = case at of
                   WithCarryIncluded -> if flagC flags' then 1 else 0
                   WithoutCarryIncluded -> 0
  put $ cpu { registers = setReg8 RegA (aVal + opVal + carryVal) regs, flags = flags' }
add WithoutCarryIncluded RegisterHL op = do
  cpu <- get
  let regs = registers cpu
  let hlVal = reg16 RegH RegL regs
  opVal <- case op of
             Reg16 _ _ -> evalOp16 op
             StackPointer Unchanged -> evalOp16 op
  let flags' = flagsFromInt16 (fromIntegral hlVal + fromIntegral opVal)
  let regs' = setReg16 RegH RegL (hlVal + opVal) regs
  put $ cpu { registers = regs', flags = flags' { flagZ = False } }
add WithoutCarryIncluded (StackPointer Unchanged) (Imm8 e8) = do
  cpu <- get
  let flags' = flagsFromInt8 (fromIntegral e8 + fromIntegral (sp cpu))
  put $ cpu { sp = offsetSP (AddInt8 e8) (sp cpu), flags = flags' }

and :: (MArray a Word8 m, AndOperands k1 k2 ~ 'KAnd) => Operand k1 -> Operand k2 -> StateT (CPU a m) m ()
and RegisterA op = do
  cpu <- get
  let regs = registers cpu
  opVal <- case op of
             Reg8 _ -> evalOp8 op
             Uimm8 _ -> evalOp8 op
             IndirectHL -> evalOp8 op
  let regs' = setReg8 RegA (reg8 RegA regs .&. opVal) regs
  let z = reg8 RegA regs' == 0
  put $ cpu { registers = regs', flags = initFlags { flagZ = z, flagH = True } }

cp :: (MArray a Word8 m, CompareOperands k1 k2 ~ 'KCompare) => Operand k1 -> Operand k2 -> StateT (CPU a m) m ()
cp RegisterA op = do
  cpu <- get
  let regs = registers cpu
  let aVal = reg8 RegA regs
  opVal <- case op of
             Reg8 _ -> evalOp8 op
             Uimm8 _ -> evalOp8 op
             IndirectHL -> evalOp8 op
  let z = (aVal - opVal) == 0
  let h = (aVal .&. 0xF) < (opVal .&. 0xF)
  let c = aVal < opVal
  put $ cpu { flags = initFlags { flagZ = z, flagH = h, flagN = True, flagC = c} }

-- TODO: implement
fetchInstruction :: Monad m => StateT (CPU a m) m Instruction
fetchInstruction = undefined

executeInstruction :: MArray a Word8 m => Instruction -> StateT (CPU a m) m ()
executeInstruction (Load o1 o2) = load o1 o2
executeInstruction (Add at o1 o2) = add at o1 o2
executeInstruction (And o1 o2) = and o1 o2
executeInstruction (Compare o1 o2) = cp o1 o2
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
