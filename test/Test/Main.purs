module Test.Main where

import Prelude

import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Console (CONSOLE())
import Control.Monad.Eff.Exception (EXCEPTION())
import Control.Monad.Eff.Random (RANDOM())
import Test.Spec.Runner (Process(), run)
import Test.Spec.Reporter.Console (consoleReporter)

import Test.Data.Char.Unicode (dataCharUnicodeTests)

main :: forall e . Eff (console :: CONSOLE, err :: EXCEPTION, process :: Process, random :: RANDOM | e) Unit
main = run [consoleReporter] $
    dataCharUnicodeTests
