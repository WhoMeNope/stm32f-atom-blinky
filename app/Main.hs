module Main where

import Atom.Language.Atom
import STM32F767ZI (compileProgram)

main :: IO ()
main = compileProgram program

-- | An example design that computes the greatest common divisor.
program :: Atom ()
program = do

  -- External reference to value A.
  let a = word32' "a"

  -- External reference to value B.
  let b = word32' "b"

  -- The external running flag.
  let running = bool' "running"

  -- A rule to modify A.
  atom "a_minus_b" $ do
    cond $ value a >. value b
    a <== value a - value b

  -- A rule to modify B.
  atom "b_minus_a" $ do
    cond $ value b >. value a
    b <== value b - value a

  -- A rule to clear the running flag.
  atom "stop" $ do
    cond $ value a ==. value b
    running <== false
