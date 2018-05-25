module Test.Data.CodePoint.Unicode (dataCharUnicodeTests) where

import Prelude

import Effect.Class (liftEffect)
import Data.Char (toCharCode)
import Data.Enum (toEnumWithDefaults, fromEnum)
import Data.Maybe (Maybe(..), fromJust)
import Data.NonEmpty ((:|))
import Data.String.CodePoints (CodePoint, codePointFromChar)
import Partial.Unsafe (unsafePartial)
import Test.QuickCheck (quickCheck)
import Test.QuickCheck.Arbitrary (class Arbitrary)
import Test.QuickCheck.Gen (Gen(), oneOf, chooseInt)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

import Data.CodePoint.Unicode ( GeneralCategory(..)
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

codePointFromInt :: Int -> CodePoint
codePointFromInt = toEnumWithDefaults bottom top

dataCharUnicodeTests :: Spec Unit
dataCharUnicodeTests = describe "module Data.CodePoint.Unicode" do
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

generalCategoryDataTypeTests :: Spec Unit
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

generalCategoryTests :: Spec Unit
generalCategoryTests = describe "generalCategory" do
    it "generalCategory (codePointFromChar 'a') == LowercaseLetter" $
        generalCategory (codePointFromChar 'a') `shouldEqual` Just LowercaseLetter
    it "generalCategory (codePointFromChar 'A') == UppercaseLetter" $
        generalCategory (codePointFromChar 'A') `shouldEqual` Just UppercaseLetter
    it "generalCategory (codePointFromChar '0') == DecimalNumber" $
        generalCategory (codePointFromChar '0') `shouldEqual` Just DecimalNumber
    it "generalCategory (codePointFromChar '%') == OtherPunctuation" $
        generalCategory (codePointFromChar '%') `shouldEqual` Just OtherPunctuation
    it "generalCategory (codePointFromChar '♥') == OtherSymbol" $
        generalCategory (codePointFromChar '♥') `shouldEqual` Just OtherSymbol
    it "generalCategory (codePointFromChar '\\31') == Control" $
        generalCategory (codePointFromChar '\31') `shouldEqual` Just Control
    it "generalCategory (codePointFromChar ' ') == Space" $
        generalCategory (codePointFromChar ' ') `shouldEqual` Just Space
    it "generalCategory (codePointFromChar '本') == OtherLetter" $
        generalCategory (codePointFromChar '本') `shouldEqual` Just OtherLetter

newtype CP = CP CodePoint
instance arbitraryCP :: Arbitrary CP where
    arbitrary = CP <<< codePointFromInt <$> chooseInt 0 0x10FFFF

newtype AsciiChar = AsciiChar CodePoint
instance arbitraryAsciiChar :: Arbitrary AsciiChar where
    arbitrary = AsciiChar <<< codePointFromInt <$> chooseInt 0 0x7F

newtype NonAsciiChar = NonAsciiChar CodePoint
instance arbitraryNonAsciiChar :: Arbitrary NonAsciiChar where
    arbitrary = NonAsciiChar <<< codePointFromInt <$> chooseInt 0x80 0xFFFF

newtype Latin1Char = Latin1Char CodePoint
instance arbitraryLatin1Char :: Arbitrary Latin1Char where
    arbitrary = Latin1Char <<< codePointFromInt <$> chooseInt 0x80 0xFF

newtype NonLatin1Char = NonLatin1Char CodePoint
instance arbitraryNonLatin1Char :: Arbitrary NonLatin1Char where
    arbitrary = NonLatin1Char <<< codePointFromInt <$> chooseInt 0x100 0xFFFF

newtype AsciiLowerChar = AsciiLowerChar CodePoint
instance arbitraryAsciiLowerChar :: Arbitrary AsciiLowerChar where
    arbitrary = AsciiLowerChar <<< codePointFromInt <$> chooseInt 0x61 0x7A

newtype NonAsciiLowerChar = NonAsciiLowerChar CodePoint
instance arbitraryNonAsciiLowerChar :: Arbitrary NonAsciiLowerChar where
    arbitrary = NonAsciiLowerChar <<< codePointFromInt <$> oneOf (g :| [g , chooseInt 0x7B 0xFFFF])
      where
        g :: Gen Int
        g = chooseInt 0 0x60

newtype AsciiUpperChar = AsciiUpperChar CodePoint
instance arbitraryAsciiUpperChar :: Arbitrary AsciiUpperChar where
    arbitrary = AsciiUpperChar <<< codePointFromInt <$> chooseInt 0x41 0x5A

newtype NonAsciiUpperChar = NonAsciiUpperChar CodePoint
instance arbitraryNonAsciiUpperChar :: Arbitrary NonAsciiUpperChar where
    arbitrary = NonAsciiUpperChar <<< codePointFromInt <$> oneOf (g :| [g , chooseInt 0x5B 0xFFFF])
      where
        g :: Gen Int
        g = chooseInt 0 0x40

newtype AsciiDigit = AsciiDigit CodePoint
instance arbitraryAsciiDigit :: Arbitrary AsciiDigit where
    arbitrary = AsciiDigit <<< codePointFromInt <$> chooseInt 0x30 0x39

newtype NonAsciiDigit = NonAsciiDigit CodePoint
instance arbitraryNonAsciiDigit :: Arbitrary NonAsciiDigit where
    arbitrary = NonAsciiDigit <<< codePointFromInt <$> oneOf (g :| [g , chooseInt 0x3A 0xFFFF])
      where
        g :: Gen Int
        g = chooseInt 0 0x2F

newtype AsciiOctDigit = AsciiOctDigit CodePoint
instance arbitraryAsciiOctDigit :: Arbitrary AsciiOctDigit where
    arbitrary = AsciiOctDigit <<< codePointFromInt <$> chooseInt 0x30 0x37

newtype NonAsciiOctDigit = NonAsciiOctDigit CodePoint
instance arbitraryNonAsciiOctDigit :: Arbitrary NonAsciiOctDigit where
    arbitrary = NonAsciiOctDigit <<< codePointFromInt <$> oneOf (g :| [g , chooseInt 0x38 0xFFFF])
      where
        g :: Gen Int
        g = chooseInt 0 0x2F

newtype AsciiHexDigit = AsciiHexDigit CodePoint
instance arbitraryAsciiHexDigit :: Arbitrary AsciiHexDigit where
    arbitrary = AsciiHexDigit <<< codePointFromInt <$> oneOf (g :| [g, chooseInt 0x41 0x46, chooseInt 0x61 0x66])
      where
        g :: Gen Int
        g = chooseInt 0x30 0x37

newtype NonAsciiHexDigit = NonAsciiHexDigit CodePoint
instance arbitraryNonAsciiHexDigit :: Arbitrary NonAsciiHexDigit where
    arbitrary = NonAsciiHexDigit <<< codePointFromInt <$> oneOf (g :| [g, chooseInt 0x3A 0x40, chooseInt 0x4A 0x60, chooseInt 0x67 0xFFFF])
      where
        g :: Gen Int
        g = chooseInt 0 0x2F

isAsciiTests :: Spec Unit
isAsciiTests = describe "isAscii" do
    it "ascii chars are ascii" $ liftEffect $ quickCheck \(AsciiChar char) -> isAscii char
    it "non ascii chars are not ascii" $ liftEffect $ quickCheck \(NonAsciiChar char) -> not $ isAscii char

isLatin1Tests :: Spec Unit
isLatin1Tests = describe "isLatin1" do
    it "ascii chars are latin1" $ liftEffect $ quickCheck \(AsciiChar char) -> isLatin1 char
    it "latin1 chars are latin1" $ liftEffect $ quickCheck \(Latin1Char char) -> isLatin1 char
    it "non latin1 chars are not latin1" $ liftEffect $ quickCheck \(NonLatin1Char char) -> not $ isLatin1 char

isAsciiLowerTests :: Spec Unit
isAsciiLowerTests = describe "isAsciiLower" do
    it "lower ascii chars are lower ascii" $ liftEffect $ quickCheck \(AsciiLowerChar char) -> isAsciiLower char
    it "non lower ascii chars are not lower ascii" $ liftEffect $ quickCheck \(NonAsciiLowerChar char) -> not $ isAsciiLower char

isAsciiUpperTests :: Spec Unit
isAsciiUpperTests = describe "isAsciiUpper" do
    it "upper ascii chars are upper ascii" $ liftEffect $ quickCheck \(AsciiUpperChar char) -> isAsciiUpper char
    it "non upper ascii chars are not upper ascii" $ liftEffect $ quickCheck \(NonAsciiUpperChar char) -> not $ isAsciiUpper char

isControlTests :: Spec Unit
isControlTests = describe "isControl" do
    it "'\\04' is Control" $
        isControl (codePointFromChar '\04') `shouldEqual` true
    it "'a' is not Control" $
        isControl (codePointFromChar 'a') `shouldEqual` false

isPrintTests :: Spec Unit
isPrintTests = describe "isPrint" do
    it "'\\04' is not Print" $
        isPrint (codePointFromChar '\04') `shouldEqual` false
    it "'\\n' is not Print" $
        isPrint (codePointFromChar '\n') `shouldEqual` false
    it "'a' is Print" $
        isPrint (codePointFromChar 'a') `shouldEqual` true
    it "' ' is Print" $
        isPrint (codePointFromChar ' ') `shouldEqual` true

isSpaceTests :: Spec Unit
isSpaceTests = describe "isSpace" do
    it "' ' is Space" $
        isSpace (codePointFromChar ' ') `shouldEqual` true
    it "'　' is Space" $
        isSpace (codePointFromChar '　') `shouldEqual` true
    it "'\\n' is Space" $
        isSpace (codePointFromChar '\n') `shouldEqual` true
    it "'\\t' is Space" $
        isSpace (codePointFromChar '\t') `shouldEqual` true
    it "'a' is not Space" $
        isSpace (codePointFromChar 'a') `shouldEqual` false

isUpperTests :: Spec Unit
isUpperTests = describe "isUpper" do
    it "'Z' is Upper" $
        isUpper (codePointFromChar 'Z') `shouldEqual` true
    it "'a' is not Upper" $
        isUpper (codePointFromChar 'a') `shouldEqual` false
    it "' ' is not Upper" $
        isUpper (codePointFromChar ' ') `shouldEqual` false
    it "'\\n' is not Upper" $
        isUpper (codePointFromChar '\n') `shouldEqual` false
    it "'日' is not Upper" $
        isUpper (codePointFromChar '日') `shouldEqual` false

isLowerTests :: Spec Unit
isLowerTests = describe "isLower" do
    it "'a' is Lower" $
        isLower (codePointFromChar 'a') `shouldEqual` true
    it "'Z' is not Lower" $
        isLower (codePointFromChar 'Z') `shouldEqual` false
    it "' ' is not Lower" $
        isLower (codePointFromChar ' ') `shouldEqual` false
    it "'\\n' is not Lower" $
        isLower (codePointFromChar '\n') `shouldEqual` false
    it "'日' is not Lower" $
        isLower (codePointFromChar '日') `shouldEqual` false

isAlphaTests :: Spec Unit
isAlphaTests = describe "isAlpha" do
    it "'a' is Alpha" $
        isAlpha (codePointFromChar 'a') `shouldEqual` true
    it "'Z' is Alpha" $
        isAlpha (codePointFromChar 'Z') `shouldEqual` true
    it "'日' is Alpha" $
        isAlpha (codePointFromChar '日') `shouldEqual` true
    it "' ' is not Alpha" $
        isAlpha (codePointFromChar ' ') `shouldEqual` false
    it "'\\n' is not Alpha" $
        isAlpha (codePointFromChar '\n') `shouldEqual` false

isAlphaNumTests :: Spec Unit
isAlphaNumTests = describe "isAlphaNum" do
    it "'a' is AlphaNum" $
        isAlphaNum (codePointFromChar 'a') `shouldEqual` true
    it "'Z' is AlphaNum" $
        isAlphaNum (codePointFromChar 'Z') `shouldEqual` true
    it "'日' is AlphaNum" $
        isAlphaNum (codePointFromChar '日') `shouldEqual` true
    it "'1' is AlphaNum" $
        isAlphaNum (codePointFromChar '1') `shouldEqual` true
    it "'２' is AlphaNum" $
        isAlphaNum (codePointFromChar '２') `shouldEqual` true
    it "'③' is AlphaNum" $
        isAlphaNum (codePointFromChar '③') `shouldEqual` true
    it "' ' is not AlphaNum" $
        isAlphaNum (codePointFromChar ' ') `shouldEqual` false
    it "'\\n' is not AlphaNum" $
        isAlphaNum (codePointFromChar '\n') `shouldEqual` false

isDigitTests :: Spec Unit
isDigitTests = describe "isDigit" do
    it "digits are digits" $ liftEffect $ quickCheck \(AsciiDigit char) -> isDigit char
    it "non digits are not digits" $ liftEffect $ quickCheck \(NonAsciiDigit char) -> not $ isDigit char

isOctDigitTests :: Spec Unit
isOctDigitTests = describe "isOctDigit" do
    it "oct digits are oct digits" $ liftEffect $ quickCheck \(AsciiOctDigit char) -> isOctDigit char
    it "non oct digits are not oct digits" $ liftEffect $ quickCheck \(NonAsciiOctDigit char) -> not $ isOctDigit char

isHexDigitTests :: Spec Unit
isHexDigitTests = describe "isHexDigit" do
    it "hex digits are hex digits" $ liftEffect $ quickCheck \(AsciiHexDigit char) -> isHexDigit char
    it "non hex digits are not hex digits" $ liftEffect $ quickCheck \(NonAsciiHexDigit char) -> not $ isHexDigit char

isPunctuationTests :: Spec Unit
isPunctuationTests = describe "isPunctuation" do
    it "'a' is not Punctuation" $
        isPunctuation (codePointFromChar 'a') `shouldEqual` false
    it "'7' is not Punctuation" $
        isPunctuation (codePointFromChar '7') `shouldEqual` false
    it "'♥' is not Punctuation" $
        isPunctuation (codePointFromChar '♥') `shouldEqual` false
    it "'日' is not Punctuation" $
        isPunctuation (codePointFromChar '日') `shouldEqual` false
    it "'\"' is Punctuation" $
        isPunctuation (codePointFromChar '"') `shouldEqual` true
    it "'?' is Punctuation" $
        isPunctuation (codePointFromChar '?') `shouldEqual` true
    it "'—' is Punctuation" $
        isPunctuation (codePointFromChar '—') `shouldEqual` true

isSymbolTests :: Spec Unit
isSymbolTests = describe "isSymbol" do
    it "'a' is not Symbol" $
        isSymbol (codePointFromChar 'a') `shouldEqual` false
    it "'6' is not Symbol" $
        isSymbol (codePointFromChar '6') `shouldEqual` false
    it "'語' is not Symbol" $
        isSymbol (codePointFromChar '語') `shouldEqual` false
    it "'-' is not Symbol" $
        isSymbol (codePointFromChar '-') `shouldEqual` false
    it "'♥' is Symbol" $
        isSymbol (codePointFromChar '♥') `shouldEqual` true
    it "'=' is Symbol" $
        isSymbol (codePointFromChar '=') `shouldEqual` true
    it "'+' is Symbol" $
        isSymbol (codePointFromChar '+') `shouldEqual` true

-- TODO: These.
toUpperTests :: Spec Unit
toUpperTests = pure unit
toLowerTests :: Spec Unit
toLowerTests = pure unit
toTitleTests :: Spec Unit
toTitleTests = pure unit

digitToIntTests :: Spec Unit
digitToIntTests = describe "digitToInt" do
    it "'0'..'9' get mapped correctly" $
        map (digitToInt <<< codePointFromChar) ['0','1','2','3','4','5','6','7','8','9'] `shouldEqual`
            [Just 0, Just 1, Just 2, Just 3, Just 4, Just 5, Just 6, Just 7, Just 8, Just 9]
    it "'a'..'f' get mapped correctly" $
        map (digitToInt <<< codePointFromChar) ['a','b','c','d','e','f'] `shouldEqual`
            [Just 10, Just 11, Just 12, Just 13, Just 14, Just 15]
    it "'A'..'F' get mapped correctly" $
        map (digitToInt <<< codePointFromChar) ['A','B','C','D','E','F'] `shouldEqual`
            [Just 10, Just 11, Just 12, Just 13, Just 14, Just 15]
    it "'G' is not a digit" $
        digitToInt (codePointFromChar 'G') `shouldEqual` Nothing
    it "'♥' is not a digit" $
        digitToInt (codePointFromChar '♥') `shouldEqual` Nothing
    it "'国' is not a digit" $
        digitToInt (codePointFromChar '国') `shouldEqual` Nothing

isLetterTests:: Spec Unit
isLetterTests = describe "isLetter" do
    it "isLetter == isAlpha" $ liftEffect $ quickCheck \(CP char) -> isLetter char == isAlpha char

isMarkTests :: Spec Unit
isMarkTests = describe "isMark" do
    -- TODO: Add a positive test here.
    it "'a' is not Mark" $
        isMark (codePointFromChar 'a') `shouldEqual` false
    it "'0' is not Mark" $
        isMark (codePointFromChar '0') `shouldEqual` false
    it "'語' is not Mark" $
        isMark (codePointFromChar '語') `shouldEqual` false
    it "'♥' is not Mark" $
        isMark (codePointFromChar '♥') `shouldEqual` false

isNumberTests :: Spec Unit
isNumberTests = describe "isNumber" do
    it "'a' is not Number" $
        isNumber (codePointFromChar 'a') `shouldEqual` false
    it "'%' is not Number" $
        isNumber (codePointFromChar '%') `shouldEqual` false
    it "'語' is not Number" $
        isNumber (codePointFromChar '語') `shouldEqual` false
    it "'♥' is not Number" $
        isNumber (codePointFromChar '♥') `shouldEqual` false
    it "'3' is Number" $
        isNumber (codePointFromChar '3') `shouldEqual` true
    it "'Ⅸ' is Number" $
        isNumber (codePointFromChar 'Ⅸ') `shouldEqual` true
    it "'３' is Number" $
        isNumber (codePointFromChar '３') `shouldEqual` true
    it "'⑳' is Number" $
        isNumber (codePointFromChar '⑳') `shouldEqual` true
    it "0..9 are Number" $ liftEffect $ quickCheck \(AsciiDigit char) -> isNumber char

isSeparatorTests :: Spec Unit
isSeparatorTests = describe "isSeparator" do
    it "'a' is not Separator" $
        isSeparator (codePointFromChar 'a') `shouldEqual` false
    it "'9' is not Separator" $
        isSeparator (codePointFromChar '9') `shouldEqual` false
    it "'\\n' is not Separator" $
        isSeparator (codePointFromChar '\n') `shouldEqual` false
    it "'\\t' is not Separator" $
        isSeparator (codePointFromChar '\t') `shouldEqual` false
    it "' ' is Separator" $
        isSeparator (codePointFromChar ' ') `shouldEqual` true
    it "'\\160' is Separator" $
        isSeparator (codePointFromChar '\160') `shouldEqual` true
