module Gameboy
    ( run
    )
where

import Data.Array.MArray
import Data.Word

import CPU hiding (step)
import qualified CPU (step)

data Gameboy a m where
  Gameboy :: MArray a Word8 m => { cpu :: CPU a m } -> Gameboy a m

loadCart :: MArray a Word8 m => m (Gameboy a m)
loadCart = Gameboy <$> initCPU

step :: MArray a Word8 m => Gameboy a m -> m (Gameboy a m)
step gameboy = do
  cpu' <- CPU.step (cpu gameboy)
  return $ gameboy { cpu = cpu' }

_run :: MArray a Word8 m => Gameboy a m -> m (Gameboy a m)
_run gameboy = do
  gameboy' <- step gameboy
  if (running . cpu) gameboy' then _run gameboy' else return gameboy'

-- TODO: actually load a cart
run :: forall a m. MArray a Word8 m => m ()
run = do
  gameboy <- loadCart @a
  _ <- _run gameboy -- TODO: check final state
  return ()
