{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module STM32F767ZI
  ( compileProgram
  ) where

import Atom.Language.Atom
import NeatInterpolation (trimming)
import Data.Text (pack, unpack)

-- | Invoke the atom compiler.
compileProgram :: Atom () -> IO ()
compileProgram program = do
  let loopName = "main_loop"
  (schedule, _, _, _, _) <- compile loopName defaults { cCode = prePostCode loopName } program
  putStrLn $ reportSchedule schedule

prePostCode :: String -> [Name] -> [Name] -> [(Name, Type)] -> (String, String)
prePostCode loopName _ _ _ =
  let loopName' = pack loopName
   in
  ( unpack $ [trimming|
      #include <stdlib.h>
      #include <stdio.h>

      unsigned long int a;
      unsigned long int b;
      unsigned long int x;
      unsigned char running = 1;
    |]
  , unpack $ [trimming|
      int main(int argc, char* argv[]) {
        if (argc < 3) {
          printf("usage: gcd <num1> <num2>\n");
        }
        else {
          a = atoi(argv[1]);
          b = atoi(argv[2]);
          printf("Computing the GCD of %lu and %lu...\n\", a, b);"
          while(running) {
            ${loopName'}();
            printf("iteration:  a = %lu  b = %lu\n\", a, b);
          }
          printf("GCD result: %lu\n\", a);
        }
        return 0;
      }
    |]
  )
