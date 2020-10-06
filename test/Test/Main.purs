module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Data.Char.Unicode (dataCharUnicodeTests)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (run, runSpec)

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  dataCharUnicodeTests
