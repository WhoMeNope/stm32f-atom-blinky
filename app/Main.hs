module Main where

import STM32F767ZI

main :: IO ()
main = compileProgram "blinky" program

program :: Atom ()
program = do
  period 1000 $ atom "blink" $ do
    call "toggleLED1"
