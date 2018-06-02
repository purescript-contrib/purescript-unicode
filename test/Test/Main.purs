module Test.Main where

import Prelude
import Test.Data.Char.Unicode (dataCharUnicodeTests)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (run)

main = run [consoleReporter] $
    dataCharUnicodeTests
