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

loadCart :: MArray a Word8 m => m (Gameboy a m)
loadCart = Gameboy <$> initCPU

step :: MArray a Word8 m => StateT (Gameboy a m) m (Gameboy a m)
step = do
  gameboy <- get
  cpu' <- lift $ CPU.step (cpu gameboy)
  let gameboy' = gameboy { cpu = cpu' }
  put gameboy' -- TODO: update PC correctly
  return gameboy'

_run :: MArray a Word8 m => StateT (Gameboy a m) m (Gameboy a m)
_run = do
  gameboy <- step
  if (running . cpu) gameboy then _run else return gameboy

-- TODO: actually load a cart
run :: forall a m. MArray a Word8 m => m ()
run = do
  gameboy <- loadCart @a
  _ <- evalStateT _run gameboy -- TODO: check final state
  return ()
