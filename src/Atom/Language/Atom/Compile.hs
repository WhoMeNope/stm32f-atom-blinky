-- | 
-- Module: Compile
-- Description: Compilation functions
-- Copyright: (c) 2013 Tom Hawkins & Lee Pike
--
-- Atom compilation functions

module Atom.Language.Atom.Compile
  ( compile
  , reportSchedule
  , Schedule
  ) where

import System.Exit
import Control.Monad (when)
import Data.Maybe (isJust)

import Atom.Language.Atom.Code
import Atom.Language.Atom.Scheduling
import Atom.Language.Atom.Elaboration
import Atom.Language.Atom.UeMap (emptyMap)
import Atom.Language.Atom.Language hiding (Atom)

-- | Compiles an atom description to C.
compile :: Name -> Config -> Atom () 
           -> IO (Schedule, RuleCoverage, [Name], [Name], [(Name, Type)])
compile name config atom' = do
  res <- elaborate emptyMap name atom'
  case res of
   Nothing -> putStrLn "ERROR: Design rule checks failed." >>
              exitWith (ExitFailure 1)
   Just (st,(state, rules, assertionNames, coverageNames, probeNames)) -> do
     let schedule' = schedule rules st
     ruleCoverage <- writeC name config state rules schedule' assertionNames
                     coverageNames probeNames
     when (isJust $ hardwareClock config) (putStrLn hwClockWarning)
     return (schedule', ruleCoverage, assertionNames, coverageNames, probeNames)

hwClockWarning :: String
hwClockWarning = unlines
 [ ""
 , "*** Atom WARNING: you are configuring to use a hardware clock.  Please remember"
 , "    to set the \"__phase_start_time\" variable to the time at which the first"
 , "    phase should be run before you enter the main Atom-generated function the"
 , "    first time."
 ]
