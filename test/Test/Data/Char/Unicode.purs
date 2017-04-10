
module Test.Data.Char.Unicode (dataCharUnicodeTests) where

import Prelude

import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE())
import Control.Monad.Eff.Exception (EXCEPTION())
import Control.Monad.Eff.Random (RANDOM())
import Data.Char (fromCharCode)
import Data.Maybe (Maybe(..))
import Data.NonEmpty ((:|))
import Test.QuickCheck (quickCheck)
import Test.QuickCheck.Arbitrary (class Arbitrary)
import Test.QuickCheck.Gen (Gen(), oneOf, chooseInt)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

import Data.Char.Unicode ( GeneralCategory(..)
                         , digitToInt
                         , generalCategory
                         , isAlpha
                         , isAlphaNum
                         , isAscii
                         , isAsciiLower
                         , isAsciiUpper
                         , isControl
                         , isDigit
                         , isHexDigit
                         , isLatin1
                         , isLetter
                         , isLower
                         , isMark
                         , isNumber
                         , isOctDigit
                         , isPrint
                         , isPunctuation
                         , isSeparator
                         , isSpace
                         , isSymbol
                         , isUpper )

dataCharUnicodeTests :: forall eff . Spec (console :: CONSOLE, random :: RANDOM, exception :: EXCEPTION | eff) Unit
dataCharUnicodeTests = describe "module Data.Char.Unicode" do
    generalCategoryDataTypeTests
    generalCategoryTests
    isAsciiTests
    isLatin1Tests
    isAsciiLowerTests
    isAsciiUpperTests
    isControlTests
    isPrintTests
    isSpaceTests
    isUpperTests
    isAlphaTests
    isAlphaNumTests
    isDigitTests
    isOctDigitTests
    isHexDigitTests
    isPunctuationTests
    isSymbolTests
    toUpperTests
    toLowerTests
    toTitleTests
    digitToIntTests
    isLetterTests
    isMarkTests
    isNumberTests
    isSeparatorTests

generalCategoryDataTypeTests :: forall eff . Spec eff Unit
generalCategoryDataTypeTests = describe "GeneralCategory instances" do
    describe "Eq instance" do
        it "UppercaseLetter == UppercaseLetter" $
            (UppercaseLetter == UppercaseLetter) `shouldEqual` true
        it "UppercaseLetter == LowercaseLetter should be false" $
            (UppercaseLetter == LowercaseLetter) `shouldEqual` false
    describe "Ord instance" $
        it "NonSpacingMark <= MathSymbol" $
            (NonSpacingMark <= MathSymbol) `shouldEqual` true
    describe "Show instance" $
        it "show EnclosingMark == \"EnclosingMark\"" $
            show EnclosingMark `shouldEqual` "EnclosingMark"
    describe "Bounded instance" do
        it "bottom == UppercaseLetter" $
            bottom `shouldEqual` UppercaseLetter
        it "top == NotAssigned" $
            top `shouldEqual` NotAssigned

generalCategoryTests :: forall eff . Spec eff Unit
generalCategoryTests = describe "generalCategory" do
    it "generalCategory 'a' == LowercaseLetter" $
        generalCategory 'a' `shouldEqual` Just LowercaseLetter
    it "generalCategory 'A' == UppercaseLetter" $
        generalCategory 'A' `shouldEqual` Just UppercaseLetter
    it "generalCategory '0' == DecimalNumber" $
        generalCategory '0' `shouldEqual` Just DecimalNumber
    it "generalCategory '%' == OtherPunctuation" $
        generalCategory '%' `shouldEqual` Just OtherPunctuation
    it "generalCategory '♥' == OtherSymbol" $
        generalCategory '♥' `shouldEqual` Just OtherSymbol
    it "generalCategory '\\31' == Control" $
        generalCategory '\31' `shouldEqual` Just Control
    it "generalCategory ' ' == Space" $
        generalCategory ' ' `shouldEqual` Just Space
    it "generalCategory '本' == OtherLetter" $
        generalCategory '本' `shouldEqual` Just OtherLetter

newtype AsciiChar = AsciiChar Char
instance arbitrayAsciiChar :: Arbitrary AsciiChar where
    arbitrary = AsciiChar <<< fromCharCode <$> chooseInt 0 0x7F

newtype NonAsciiChar = NonAsciiChar Char
instance arbitrayNonAsciiChar :: Arbitrary NonAsciiChar where
    arbitrary = NonAsciiChar <<< fromCharCode <$> chooseInt 0x80 0xFFFF

newtype Latin1Char = Latin1Char Char
instance arbitrayLatin1Char :: Arbitrary Latin1Char where
    arbitrary = Latin1Char <<< fromCharCode <$> chooseInt 0x80 0xFF

newtype NonLatin1Char = NonLatin1Char Char
instance arbitrayNonLatin1Char :: Arbitrary NonLatin1Char where
    arbitrary = NonLatin1Char <<< fromCharCode <$> chooseInt 0x100 0xFFFF

newtype AsciiLowerChar = AsciiLowerChar Char
instance arbitrayAsciiLowerChar :: Arbitrary AsciiLowerChar where
    arbitrary = AsciiLowerChar <<< fromCharCode <$> chooseInt 0x61 0x7A

newtype NonAsciiLowerChar = NonAsciiLowerChar Char
instance arbitrayNonAsciiLowerChar :: Arbitrary NonAsciiLowerChar where
    arbitrary = NonAsciiLowerChar <<< fromCharCode <$> oneOf (g :| [g , chooseInt 0x7B 0xFFFF])
      where
        g :: Gen Int
        g = chooseInt 0 0x60

newtype AsciiUpperChar = AsciiUpperChar Char
instance arbitrayAsciiUpperChar :: Arbitrary AsciiUpperChar where
    arbitrary = AsciiUpperChar <<< fromCharCode <$> chooseInt 0x41 0x5A

newtype NonAsciiUpperChar = NonAsciiUpperChar Char
instance arbitrayNonAsciiUpperChar :: Arbitrary NonAsciiUpperChar where
    arbitrary = NonAsciiUpperChar <<< fromCharCode <$> oneOf (g :| [g , chooseInt 0x5B 0xFFFF])
      where
        g :: Gen Int
        g = chooseInt 0 0x40

newtype AsciiDigit = AsciiDigit Char
instance arbitrayAsciiDigit :: Arbitrary AsciiDigit where
    arbitrary = AsciiDigit <<< fromCharCode <$> chooseInt 0x30 0x39

newtype NonAsciiDigit = NonAsciiDigit Char
instance arbitrayNonAsciiDigit :: Arbitrary NonAsciiDigit where
    arbitrary = NonAsciiDigit <<< fromCharCode <$> oneOf (g :| [g , chooseInt 0x3A 0xFFFF])
      where
        g :: Gen Int
        g = chooseInt 0 0x2F

newtype AsciiOctDigit = AsciiOctDigit Char
instance arbitrayAsciiOctDigit :: Arbitrary AsciiOctDigit where
    arbitrary = AsciiOctDigit <<< fromCharCode <$> chooseInt 0x30 0x37

newtype NonAsciiOctDigit = NonAsciiOctDigit Char
instance arbitrayNonAsciiOctDigit :: Arbitrary NonAsciiOctDigit where
    arbitrary = NonAsciiOctDigit <<< fromCharCode <$> oneOf (g :| [g , chooseInt 0x38 0xFFFF])
      where
        g :: Gen Int
        g = chooseInt 0 0x2F

newtype AsciiHexDigit = AsciiHexDigit Char
instance arbitrayAsciiHexDigit :: Arbitrary AsciiHexDigit where
    arbitrary = AsciiHexDigit <<< fromCharCode <$> oneOf (g :| [g, chooseInt 0x41 0x46, chooseInt 0x61 0x66])
      where
        g :: Gen Int
        g = chooseInt 0x30 0x37

newtype NonAsciiHexDigit = NonAsciiHexDigit Char
instance arbitrayNonAsciiHexDigit :: Arbitrary NonAsciiHexDigit where
    arbitrary = NonAsciiHexDigit <<< fromCharCode <$> oneOf (g :| [g, chooseInt 0x3A 0x40, chooseInt 0x4A 0x60, chooseInt 0x67 0xFFFF])
      where
        g :: Gen Int
        g = chooseInt 0 0x2F

isAsciiTests :: forall eff . Spec (console :: CONSOLE, random :: RANDOM, exception :: EXCEPTION | eff) Unit
isAsciiTests = describe "isAscii" do
    it "ascii chars are ascii" $ liftEff $ quickCheck \(AsciiChar char) -> isAscii char
    it "non ascii chars are not ascii" $ liftEff $ quickCheck \(NonAsciiChar char) -> not $ isAscii char

isLatin1Tests :: forall eff . Spec (console :: CONSOLE, random :: RANDOM, exception :: EXCEPTION | eff) Unit
isLatin1Tests = describe "isLatin1" do
    it "ascii chars are latin1" $ liftEff $ quickCheck \(AsciiChar char) -> isLatin1 char
    it "latin1 chars are latin1" $ liftEff $ quickCheck \(Latin1Char char) -> isLatin1 char
    it "non latin1 chars are not latin1" $ liftEff $ quickCheck \(NonLatin1Char char) -> not $ isLatin1 char

isAsciiLowerTests :: forall eff . Spec (console :: CONSOLE, random :: RANDOM, exception :: EXCEPTION | eff) Unit
isAsciiLowerTests = describe "isAsciiLower" do
    it "lower ascii chars are lower ascii" $ liftEff $ quickCheck \(AsciiLowerChar char) -> isAsciiLower char
    it "non lower ascii chars are not lower ascii" $ liftEff $ quickCheck \(NonAsciiLowerChar char) -> not $ isAsciiLower char

isAsciiUpperTests :: forall eff . Spec (console :: CONSOLE, random :: RANDOM, exception :: EXCEPTION | eff) Unit
isAsciiUpperTests = describe "isAsciiUpper" do
    it "upper ascii chars are upper ascii" $ liftEff $ quickCheck \(AsciiUpperChar char) -> isAsciiUpper char
    it "non upper ascii chars are not upper ascii" $ liftEff $ quickCheck \(NonAsciiUpperChar char) -> not $ isAsciiUpper char

isControlTests :: forall eff . Spec eff Unit
isControlTests = describe "isControl" do
    it "'\\04' is Control" $
        isControl '\04' `shouldEqual` true
    it "'a' is not Control" $
        isControl 'a' `shouldEqual` false

isPrintTests :: forall eff . Spec eff Unit
isPrintTests = describe "isPrint" do
    it "'\\04' is not Print" $
        isPrint '\04' `shouldEqual` false
    it "'\\n' is not Print" $
        isPrint '\n' `shouldEqual` false
    it "'a' is Print" $
        isPrint 'a' `shouldEqual` true
    it "' ' is Print" $
        isPrint ' ' `shouldEqual` true

isSpaceTests :: forall eff . Spec eff Unit
isSpaceTests = describe "isSpace" do
    it "' ' is Space" $
        isSpace ' ' `shouldEqual` true
    it "'　' is Space" $
        isSpace '　' `shouldEqual` true
    it "'\\n' is Space" $
        isSpace '\n' `shouldEqual` true
    it "'\\t' is Space" $
        isSpace '\t' `shouldEqual` true
    it "'a' is not Space" $
        isSpace 'a' `shouldEqual` false

isUpperTests :: forall eff . Spec eff Unit
isUpperTests = describe "isUpper" do
    it "'Z' is Upper" $
        isUpper 'Z' `shouldEqual` true
    it "'a' is not Upper" $
        isUpper 'a' `shouldEqual` false
    it "' ' is not Upper" $
        isUpper ' ' `shouldEqual` false
    it "'\\n' is not Upper" $
        isUpper '\n' `shouldEqual` false
    it "'日' is not Upper" $
        isUpper '日' `shouldEqual` false

isLowerTests :: forall eff . Spec eff Unit
isLowerTests = describe "isLower" do
    it "'a' is Lower" $
        isLower 'a' `shouldEqual` true
    it "'Z' is not Lower" $
        isLower 'Z' `shouldEqual` false
    it "' ' is not Lower" $
        isLower ' ' `shouldEqual` false
    it "'\\n' is not Lower" $
        isLower '\n' `shouldEqual` false
    it "'日' is not Lower" $
        isLower '日' `shouldEqual` false

isAlphaTests :: forall eff . Spec eff Unit
isAlphaTests = describe "isAlpha" do
    it "'a' is Alpha" $
        isAlpha 'a' `shouldEqual` true
    it "'Z' is Alpha" $
        isAlpha 'Z' `shouldEqual` true
    it "'日' is Alpha" $
        isAlpha '日' `shouldEqual` true
    it "' ' is not Alpha" $
        isAlpha ' ' `shouldEqual` false
    it "'\\n' is not Alpha" $
        isAlpha '\n' `shouldEqual` false

isAlphaNumTests :: forall eff . Spec eff Unit
isAlphaNumTests = describe "isAlphaNum" do
    it "'a' is AlphaNum" $
        isAlphaNum 'a' `shouldEqual` true
    it "'Z' is AlphaNum" $
        isAlphaNum 'Z' `shouldEqual` true
    it "'日' is AlphaNum" $
        isAlphaNum '日' `shouldEqual` true
    it "'1' is AlphaNum" $
        isAlphaNum '1' `shouldEqual` true
    it "'２' is AlphaNum" $
        isAlphaNum '２' `shouldEqual` true
    it "'③' is AlphaNum" $
        isAlphaNum '③' `shouldEqual` true
    it "' ' is not AlphaNum" $
        isAlphaNum ' ' `shouldEqual` false
    it "'\\n' is not AlphaNum" $
        isAlphaNum '\n' `shouldEqual` false

isDigitTests :: forall eff . Spec (console :: CONSOLE, random :: RANDOM, exception :: EXCEPTION | eff) Unit
isDigitTests = describe "isDigit" do
    it "digits are digits" $ liftEff $ quickCheck \(AsciiDigit char) -> isDigit char
    it "non digits are not digits" $ liftEff $ quickCheck \(NonAsciiDigit char) -> not $ isDigit char

isOctDigitTests :: forall eff . Spec (console :: CONSOLE, random :: RANDOM, exception :: EXCEPTION | eff) Unit
isOctDigitTests = describe "isOctDigit" do
    it "oct digits are oct digits" $ liftEff $ quickCheck \(AsciiOctDigit char) -> isOctDigit char
    it "non oct digits are not oct digits" $ liftEff $ quickCheck \(NonAsciiOctDigit char) -> not $ isOctDigit char

isHexDigitTests :: forall eff . Spec (console :: CONSOLE, random :: RANDOM, exception :: EXCEPTION | eff) Unit
isHexDigitTests = describe "isHexDigit" do
    it "hex digits are hex digits" $ liftEff $ quickCheck \(AsciiHexDigit char) -> isHexDigit char
    it "non hex digits are not hex digits" $ liftEff $ quickCheck \(NonAsciiHexDigit char) -> not $ isHexDigit char

isPunctuationTests :: forall eff . Spec eff Unit
isPunctuationTests = describe "isPunctuation" do
    it "'a' is not Punctuation" $
        isPunctuation 'a' `shouldEqual` false
    it "'7' is not Punctuation" $
        isPunctuation '7' `shouldEqual` false
    it "'♥' is not Punctuation" $
        isPunctuation '♥' `shouldEqual` false
    it "'日' is not Punctuation" $
        isPunctuation '日' `shouldEqual` false
    it "'\"' is Punctuation" $
        isPunctuation '"' `shouldEqual` true
    it "'?' is Punctuation" $
        isPunctuation '?' `shouldEqual` true
    it "'—' is Punctuation" $
        isPunctuation '—' `shouldEqual` true

isSymbolTests :: forall eff . Spec eff Unit
isSymbolTests = describe "isSymbol" do
    it "'a' is not Symbol" $
        isSymbol 'a' `shouldEqual` false
    it "'6' is not Symbol" $
        isSymbol '6' `shouldEqual` false
    it "'語' is not Symbol" $
        isSymbol '語' `shouldEqual` false
    it "'-' is not Symbol" $
        isSymbol '-' `shouldEqual` false
    it "'♥' is Symbol" $
        isSymbol '♥' `shouldEqual` true
    it "'=' is Symbol" $
        isSymbol '=' `shouldEqual` true
    it "'+' is Symbol" $
        isSymbol '+' `shouldEqual` true

-- TODO: These.
toUpperTests :: forall eff . Spec eff Unit
toUpperTests = pure unit
toLowerTests :: forall eff . Spec eff Unit
toLowerTests = pure unit
toTitleTests :: forall eff . Spec eff Unit
toTitleTests = pure unit

digitToIntTests :: forall eff . Spec eff Unit
digitToIntTests = describe "digitToInt" do
    it "'0'..'9' get mapped correctly" $
        map digitToInt ['0','1','2','3','4','5','6','7','8','9'] `shouldEqual`
            [Just 0, Just 1, Just 2, Just 3, Just 4, Just 5, Just 6, Just 7, Just 8, Just 9]
    it "'a'..'f' get mapped correctly" $
        map digitToInt ['a','b','c','d','e','f'] `shouldEqual`
            [Just 10, Just 11, Just 12, Just 13, Just 14, Just 15]
    it "'A'..'F' get mapped correctly" $
        map digitToInt ['A','B','C','D','E','F'] `shouldEqual`
            [Just 10, Just 11, Just 12, Just 13, Just 14, Just 15]
    it "'G' is not a digit" $
        digitToInt 'G' `shouldEqual` Nothing
    it "'♥' is not a digit" $
        digitToInt '♥' `shouldEqual` Nothing
    it "'国' is not a digit" $
        digitToInt '国' `shouldEqual` Nothing

isLetterTests:: forall eff . Spec (console :: CONSOLE, random :: RANDOM, exception :: EXCEPTION | eff) Unit
isLetterTests = describe "isLetter" do
    it "isLetter == isAlpha" $ liftEff $ quickCheck \char -> isLetter char == isAlpha char

isMarkTests :: forall eff . Spec eff Unit
isMarkTests = describe "isMark" do
    -- TODO: Add a positive test here.
    it "'a' is not Mark" $
        isMark 'a' `shouldEqual` false
    it "'0' is not Mark" $
        isMark '0' `shouldEqual` false
    it "'語' is not Mark" $
        isMark '語' `shouldEqual` false
    it "'♥' is not Mark" $
        isMark '♥' `shouldEqual` false

isNumberTests :: forall eff . Spec (console :: CONSOLE, random :: RANDOM, exception :: EXCEPTION | eff) Unit
isNumberTests = describe "isNumber" do
    it "'a' is not Number" $
        isNumber 'a' `shouldEqual` false
    it "'%' is not Number" $
        isNumber '%' `shouldEqual` false
    it "'語' is not Number" $
        isNumber '語' `shouldEqual` false
    it "'♥' is not Number" $
        isNumber '♥' `shouldEqual` false
    it "'3' is Number" $
        isNumber '3' `shouldEqual` true
    it "'Ⅸ' is Number" $
        isNumber 'Ⅸ' `shouldEqual` true
    it "'３' is Number" $
        isNumber '３' `shouldEqual` true
    it "'⑳' is Number" $
        isNumber '⑳' `shouldEqual` true
    it "0..9 are Number" $ liftEff $ quickCheck \(AsciiDigit char) -> isNumber char

isSeparatorTests :: forall eff . Spec eff Unit
isSeparatorTests = describe "isSeparator" do
    it "'a' is not Separator" $
        isSeparator 'a' `shouldEqual` false
    it "'9' is not Separator" $
        isSeparator '9' `shouldEqual` false
    it "'\\n' is not Separator" $
        isSeparator '\n' `shouldEqual` false
    it "'\\t' is not Separator" $
        isSeparator '\t' `shouldEqual` false
    it "' ' is Separator" $
        isSeparator ' ' `shouldEqual` true
    it "'\\160' is Separator" $
        isSeparator '\160' `shouldEqual` true
