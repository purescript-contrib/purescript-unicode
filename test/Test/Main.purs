module Test.Main where

import Prelude

import Control.Monad.Reader.Trans (runReaderT)
import Effect (Effect)
import Test.Data.CodePoint.Unicode (dataCharUnicodeTests)

main :: Effect Unit
main =
  runReaderT dataCharUnicodeTests 0
