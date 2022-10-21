module Main where

import Data.Array.IO

import Gameboy

main :: IO ()
main = run @_ @IOArray
