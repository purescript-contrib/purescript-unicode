module Data.String.Unicode
  ( toUpper
  , toLower
  , caseFold
  , caselessMatch
  , toUpperSimple
  , toLowerSimple
  , caseFoldSimple
  ) where

import Prelude

import Control.Bind (bindFlipped)
import Data.CodePoint.Unicode as CP
import Data.String (CodePoint, fromCodePointArray, toCodePointArray)
import Unsafe.Coerce (unsafeCoerce)

-- Full Unicode conversions

-- | Convert each code point in the string to its corresponding uppercase
-- | sequence. This is the full (locale-independent) Unicode algorithm,
-- | and may map single code points to more than one code point. For example,
-- | `toUpper "ß" == "SS"`.
-- |
-- | Because this matches on more rules, it may be slower than `toUpper`, but it
-- | provides more correct results.
toUpper :: String -> String
toUpper = convFull CP.toUpper

-- | Convert each code point in the string to its corresponding lower
-- | sequence. This is the full (locale-independent) Unicode algorithm,
-- | and may map single code points to more than one code point. For example,
-- | `toLower "\x0130" == "\x0069\x0307"`.
-- |
-- | Because this matches on more rules, it may be slower than `toLower`, but it
-- | provides more correct results.
toLower :: String -> String
toLower = convFull CP.toLower

-- | The full Unicode case folding algorithm, may increase the length of the
-- | string by mapping individual code points to longer sequences.
caseFold :: String -> String
caseFold = convFull CP.caseFold

-- | Caseless matching, based on `caseFold`.
caselessMatch :: String -> String -> Boolean
caselessMatch s1 s2 = caseFold s1 == caseFold s2

-- Simple code-point-to-code-point conversion algorithms

-- | Convert each code point in the string to its corresponding uppercase
-- | code point.
-- |
-- | Note: this is not the full Unicode algorithm, see `toUpper`.
toUpperSimple :: String -> String
toUpperSimple = conv CP.toUpperSimple

-- | Convert each code point in the string to its corresponding lowercase
-- | code point.
-- |
-- | Note: this is not the full Unicode algorithm, see `toLower`.
toLowerSimple :: String -> String
toLowerSimple = conv CP.toLowerSimple

-- | Code-point-to-code-point case folding. May be faster than `caseFold`.
caseFoldSimple :: String -> String
caseFoldSimple = conv CP.caseFoldSimple

-- | Convert a letter to the corresponding title-case or upper-case
-- | letter, if any.  (Title case differs from upper case only for a small
-- | number of ligature letters.)
-- | Any other character is returned unchanged.
--toTitle :: CodePoint -> CodePoint
--toTitle = conv CP.toTitle

-- Helper functions

modify :: (Int -> Int) -> (CodePoint -> CodePoint)
modify = unsafeCoerce

modifyFull :: (Int -> Array Int) -> (CodePoint -> Array CodePoint)
modifyFull = unsafeCoerce

conv :: (CodePoint -> CodePoint) -> String -> String
conv f = toCodePointArray >>> map f >>> fromCodePointArray

convFull :: (CodePoint -> Array CodePoint) -> String -> String
convFull f = toCodePointArray >>> bindFlipped f >>> fromCodePointArray
