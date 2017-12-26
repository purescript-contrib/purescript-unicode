module Test.Main where

import Prelude
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Random (RANDOM)
import Control.Monad.Eff.Timer (TIMER)
import Node.Process (PROCESS)
import Test.Data.CodePoint.Unicode (dataCharUnicodeTests)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (run)

main :: forall e . Eff (avar :: AVAR, console :: CONSOLE, exception :: EXCEPTION, process :: PROCESS, random :: RANDOM, timer :: TIMER | e) Unit
main = run [consoleReporter] $
    dataCharUnicodeTests
