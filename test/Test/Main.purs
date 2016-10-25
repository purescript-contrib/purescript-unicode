module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Random (RANDOM)
import Node.Process (PROCESS)
import Test.Data.Char.Unicode (dataCharUnicodeTests)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (run)

main :: forall e . Eff (console :: CONSOLE, err :: EXCEPTION, process :: PROCESS, random :: RANDOM | e) Unit
main = run [consoleReporter] $
    dataCharUnicodeTests
