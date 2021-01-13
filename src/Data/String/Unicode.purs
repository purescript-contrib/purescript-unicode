module Data.String.Unicode where

import Prelude

import Control.Bind (bindFlipped)
import Data.CodePoint.Unicode as CP
import Data.CodePoint.Unicode.Casing as Casing
import Data.String (CodePoint, fromCodePointArray, toCodePointArray)
import Unsafe.Coerce (unsafeCoerce)

modifyFull :: (Int -> Array Int) -> (CodePoint -> Array CodePoint)
modifyFull = unsafeCoerce

conv :: (CodePoint -> CodePoint) -> String -> String
conv f = toCodePointArray >>> map f >>> fromCodePointArray

convFull :: (CodePoint -> Array CodePoint) -> String -> String
convFull f = toCodePointArray >>> bindFlipped f >>> fromCodePointArray

-- | Convert a letter to the corresponding upper-case letter, if any.
-- | Any other character is returned unchanged.
toUpper :: String -> String
toUpper = conv CP.toUpper

toUpperFull :: String -> String
toUpperFull = convFull (modifyFull Casing.upper)

-- | Convert a letter to the corresponding lower-case letter, if any.
-- | Any other character is returned unchanged.
toLower :: String -> String
toLower = conv CP.toLower

toLowerFull :: String -> String
toLowerFull = convFull (modifyFull Casing.lower)

-- | Convert a letter to the corresponding title-case or upper-case
-- | letter, if any.  (Title case differs from upper case only for a small
-- | number of ligature letters.)
-- | Any other character is returned unchanged.
--toTitle :: CodePoint -> CodePoint
--toTitle = conv CP.toTitle
