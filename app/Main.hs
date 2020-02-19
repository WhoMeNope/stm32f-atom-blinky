module Main where

import STM32F767ZI

main :: IO ()
main = compileProgram "blinky" program

program :: Atom ()
program = do
  let led1 = led' "led1" False

  period 1000 $ atom "blink" $ do
    led1 <== (not $ value led1)
