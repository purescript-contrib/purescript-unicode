module Test.Main where

import Prelude

import Control.Monad (when)
import Control.Monad.Aff (Aff())
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE(), log)
import Control.Monad.Eff.Exception (EXCEPTION())
import Control.Monad.Eff.Random (RANDOM())
import Data.Char (fromCharCode)
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.String (fromChar)
import Test.Spec (Spec(), describe, it)
import Test.Spec.Runner (Process(), run)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)

import Test.Data.Char.Unicode (dataCharUnicodeTests)

main :: forall e . Eff (console :: CONSOLE, err :: EXCEPTION, process :: Process, random :: RANDOM | e) Unit
main = run [consoleReporter] $
    dataCharUnicodeTests
