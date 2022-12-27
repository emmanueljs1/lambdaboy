module Gameboy
    ( run
    )
where

import Control.Monad.State.Lazy
import Data.Array.MArray
import Data.Word

import CPU hiding (step)
import qualified CPU (step)

data Gameboy a m where
  Gameboy :: MArray a Word8 m => { cpu :: CPU a m } -> Gameboy a m

-- TODO: actually load a cart
loadCart :: MArray a Word8 m => m (Gameboy a m)
loadCart = Gameboy <$> initCPU

step :: MArray a Word8 m => StateT (Gameboy a m) m ()
step = do
  gb <- get
  cpu' <- lift $ execStateT CPU.step (cpu gb)
  put $ gb { cpu = cpu' }

_run :: MArray a Word8 m => StateT (Gameboy a m) m ()
_run = do
  step
  gb <- get
  when ((running . cpu) gb) _run

run :: forall a m. MArray a Word8 m => m ()
run = do
  gameboy <- loadCart @a
  _ <- execStateT _run gameboy
  return ()
