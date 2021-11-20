module Test.Data.CodePoint.Unicode (dataCharUnicodeTests) where

import Prelude

import Control.Monad.Reader.Class (class MonadReader, ask, local)
import Data.Array.NonEmpty as NEA
import Data.CodePoint.Unicode (GeneralCategory(..), generalCategory, hexDigitToInt, isAlpha, isAlphaNum, isAscii, isAsciiLower, isAsciiUpper, isControl, isDecDigit, isHexDigit, isLatin1, isLetter, isLower, isMark, isNumber, isOctDigit, isPrint, isPunctuation, isSeparator, isSpace, isSymbol, isUpper)
import Data.Enum (toEnumWithDefaults)
import Data.Maybe (Maybe(..))
import Data.Monoid (power, guard)
import Data.String (CodePoint, codePointFromChar)
import Data.String.Unicode (toLowerSimple, toLower, toUpperSimple, toUpper)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console (log)
import Test.Assert (assertEqual)
import Test.QuickCheck (quickCheck)
import Test.QuickCheck.Arbitrary (class Arbitrary)
import Test.QuickCheck.Gen (Gen, oneOf, chooseInt)

-----------------------------------------------------------------

-- Provide similar API to purescript-spec to reduce code changes

describe :: forall m. MonadReader Int m => MonadEffect m => String -> m Unit -> m Unit
describe msg runTest = do
  indentation <- ask
  let spacing = guard (indentation > 0) " "
  liftEffect $ log $ (power ">>" indentation) <> spacing <> msg
  local (_ + 1) runTest

it :: forall m. MonadReader Int m => MonadEffect m => String -> m Unit -> m Unit
it = describe

shouldEqual :: forall m a. MonadEffect m => Eq a => Show a => a -> a -> m Unit
shouldEqual actual expected =
  liftEffect $ assertEqual { actual, expected }

-----------------------------------------------------------------

codePointFromInt :: Int -> CodePoint
codePointFromInt = toEnumWithDefaults bottom top

dataCharUnicodeTests :: forall m. MonadReader Int m => MonadEffect m => m Unit
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
  isLowerTests
  isAlphaTests
  isAlphaNumTests
  isDecDigitTests
  isOctDigitTests
  isHexDigitTests
  isPunctuationTests
  isSymbolTests
  toUpperTests
  toLowerTests
  toTitleTests
  hexDigitToIntTests
  isLetterTests
  isMarkTests
  isNumberTests
  isSeparatorTests

generalCategoryDataTypeTests :: forall m. MonadReader Int m => MonadEffect m => m Unit
generalCategoryDataTypeTests = describe "GeneralCategory instances" do
  describe "Eq instance" do
    it "UppercaseLetter == UppercaseLetter" $
      (UppercaseLetter == UppercaseLetter) `shouldEqual` true
    it "UppercaseLetter == LowercaseLetter should be false" $
      (UppercaseLetter == LowercaseLetter) `shouldEqual` false
  describe "Ord instance"
    $ it "NonSpacingMark <= MathSymbol"
    $
      (NonSpacingMark <= MathSymbol) `shouldEqual` true
  describe "Show instance"
    $ it "show EnclosingMark == \"EnclosingMark\""
    $
      show EnclosingMark `shouldEqual` "EnclosingMark"
  describe "Bounded instance" do
    it "bottom == UppercaseLetter" $
      bottom `shouldEqual` UppercaseLetter
    it "top == NotAssigned" $
      top `shouldEqual` NotAssigned

generalCategoryTests :: forall m. MonadReader Int m => MonadEffect m => m Unit
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
  it "generalCategory (codePointFromChar '\\x1F') == Control" $
    generalCategory (codePointFromChar '\x1F') `shouldEqual` Just Control
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
  arbitrary = NonAsciiLowerChar <<< codePointFromInt <$> oneOf (g `NEA.cons'` [ g, chooseInt 0x7B 0xFFFF ])
    where
    g :: Gen Int
    g = chooseInt 0 0x60

newtype AsciiUpperChar = AsciiUpperChar CodePoint

instance arbitraryAsciiUpperChar :: Arbitrary AsciiUpperChar where
  arbitrary = AsciiUpperChar <<< codePointFromInt <$> chooseInt 0x41 0x5A

newtype NonAsciiUpperChar = NonAsciiUpperChar CodePoint

instance arbitraryNonAsciiUpperChar :: Arbitrary NonAsciiUpperChar where
  arbitrary = NonAsciiUpperChar <<< codePointFromInt <$> oneOf (g `NEA.cons'` [ g, chooseInt 0x5B 0xFFFF ])
    where
    g :: Gen Int
    g = chooseInt 0 0x40

newtype AsciiDecDigit = AsciiDecDigit CodePoint

instance arbitraryAsciiDecDigit :: Arbitrary AsciiDecDigit where
  arbitrary = AsciiDecDigit <<< codePointFromInt <$> chooseInt 0x30 0x39

newtype NonAsciiDecDigit = NonAsciiDecDigit CodePoint

instance arbitraryNonAsciiDecDigit :: Arbitrary NonAsciiDecDigit where
  arbitrary = NonAsciiDecDigit <<< codePointFromInt <$> oneOf (g `NEA.cons'` [ g, chooseInt 0x3A 0xFFFF ])
    where
    g :: Gen Int
    g = chooseInt 0 0x2F

newtype AsciiOctDigit = AsciiOctDigit CodePoint

instance arbitraryAsciiOctDigit :: Arbitrary AsciiOctDigit where
  arbitrary = AsciiOctDigit <<< codePointFromInt <$> chooseInt 0x30 0x37

newtype NonAsciiOctDigit = NonAsciiOctDigit CodePoint

instance arbitraryNonAsciiOctDigit :: Arbitrary NonAsciiOctDigit where
  arbitrary = NonAsciiOctDigit <<< codePointFromInt <$> oneOf (g `NEA.cons'` [ g, chooseInt 0x38 0xFFFF ])
    where
    g :: Gen Int
    g = chooseInt 0 0x2F

newtype AsciiHexDigit = AsciiHexDigit CodePoint

instance arbitraryAsciiHexDigit :: Arbitrary AsciiHexDigit where
  arbitrary = AsciiHexDigit <<< codePointFromInt <$> oneOf (g `NEA.cons'` [ g, chooseInt 0x41 0x46, chooseInt 0x61 0x66 ])
    where
    g :: Gen Int
    g = chooseInt 0x30 0x37

newtype NonAsciiHexDigit = NonAsciiHexDigit CodePoint

instance arbitraryNonAsciiHexDigit :: Arbitrary NonAsciiHexDigit where
  arbitrary = NonAsciiHexDigit <<< codePointFromInt <$> oneOf (g `NEA.cons'` [ g, chooseInt 0x3A 0x40, chooseInt 0x4A 0x60, chooseInt 0x67 0xFFFF ])
    where
    g :: Gen Int
    g = chooseInt 0 0x2F

isAsciiTests :: forall m. MonadReader Int m => MonadEffect m => m Unit
isAsciiTests = describe "isAscii" do
  it "ascii chars are ascii" $ liftEffect $ quickCheck \(AsciiChar char) -> isAscii char
  it "non ascii chars are not ascii" $ liftEffect $ quickCheck \(NonAsciiChar char) -> not $ isAscii char

isLatin1Tests :: forall m. MonadReader Int m => MonadEffect m => m Unit
isLatin1Tests = describe "isLatin1" do
  it "ascii chars are latin1" $ liftEffect $ quickCheck \(AsciiChar char) -> isLatin1 char
  it "latin1 chars are latin1" $ liftEffect $ quickCheck \(Latin1Char char) -> isLatin1 char
  it "non latin1 chars are not latin1" $ liftEffect $ quickCheck \(NonLatin1Char char) -> not $ isLatin1 char

isAsciiLowerTests :: forall m. MonadReader Int m => MonadEffect m => m Unit
isAsciiLowerTests = describe "isAsciiLower" do
  it "lower ascii chars are lower ascii" $ liftEffect $ quickCheck \(AsciiLowerChar char) -> isAsciiLower char
  it "non lower ascii chars are not lower ascii" $ liftEffect $ quickCheck \(NonAsciiLowerChar char) -> not $ isAsciiLower char

isAsciiUpperTests :: forall m. MonadReader Int m => MonadEffect m => m Unit
isAsciiUpperTests = describe "isAsciiUpper" do
  it "upper ascii chars are upper ascii" $ liftEffect $ quickCheck \(AsciiUpperChar char) -> isAsciiUpper char
  it "non upper ascii chars are not upper ascii" $ liftEffect $ quickCheck \(NonAsciiUpperChar char) -> not $ isAsciiUpper char

isControlTests :: forall m. MonadReader Int m => MonadEffect m => m Unit
isControlTests = describe "isControl" do
  it "'\\x04' is Control" $
    isControl (codePointFromChar '\x04') `shouldEqual` true
  it "'a' is not Control" $
    isControl (codePointFromChar 'a') `shouldEqual` false

isPrintTests :: forall m. MonadReader Int m => MonadEffect m => m Unit
isPrintTests = describe "isPrint" do
  it "'\\x04' is not Print" $
    isPrint (codePointFromChar '\x04') `shouldEqual` false
  it "'\\n' is not Print" $
    isPrint (codePointFromChar '\n') `shouldEqual` false
  it "'a' is Print" $
    isPrint (codePointFromChar 'a') `shouldEqual` true
  it "' ' is Print" $
    isPrint (codePointFromChar ' ') `shouldEqual` true

isSpaceTests :: forall m. MonadReader Int m => MonadEffect m => m Unit
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
  it "'日' is not Space" $
    isSpace (codePointFromChar '日') `shouldEqual` false

isUpperTests :: forall m. MonadReader Int m => MonadEffect m => m Unit
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

isLowerTests :: forall m. MonadReader Int m => MonadEffect m => m Unit
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

isAlphaTests :: forall m. MonadReader Int m => MonadEffect m => m Unit
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

isAlphaNumTests :: forall m. MonadReader Int m => MonadEffect m => m Unit
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

isDecDigitTests :: forall m. MonadReader Int m => MonadEffect m => m Unit
isDecDigitTests = describe "isDecDigit" do
  it "digits are digits" $ liftEffect $ quickCheck \(AsciiDecDigit char) -> isDecDigit char
  it "non digits are not digits" $ liftEffect $ quickCheck \(NonAsciiDecDigit char) -> not $ isDecDigit char

isOctDigitTests :: forall m. MonadReader Int m => MonadEffect m => m Unit
isOctDigitTests = describe "isOctDigit" do
  it "oct digits are oct digits" $ liftEffect $ quickCheck \(AsciiOctDigit char) -> isOctDigit char
  it "non oct digits are not oct digits" $ liftEffect $ quickCheck \(NonAsciiOctDigit char) -> not $ isOctDigit char

isHexDigitTests :: forall m. MonadReader Int m => MonadEffect m => m Unit
isHexDigitTests = describe "isHexDigit" do
  it "hex digits are hex digits" $ liftEffect $ quickCheck \(AsciiHexDigit char) -> isHexDigit char
  it "non hex digits are not hex digits" $ liftEffect $ quickCheck \(NonAsciiHexDigit char) -> not $ isHexDigit char

isPunctuationTests :: forall m. MonadReader Int m => MonadEffect m => m Unit
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

isSymbolTests :: forall m. MonadReader Int m => MonadEffect m => m Unit
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

toUpperTests :: forall m. MonadReader Int m => MonadEffect m => m Unit
toUpperTests = describe "toUpperSimple && toUpper" do
  let accente = "\xE9"
  let accentE = "\xC9"
  it "should handle precomposed e with accent \xE9 -> \xC9" do
    toUpperSimple accente `shouldEqual` accentE
    toUpper accente `shouldEqual` accentE
  it "should not affect emoji" do
    let emoji = "😃🧘🏻‍♂️🌍🍞🚗📞🎉♥️🏁"
    toUpperSimple emoji `shouldEqual` emoji
    toUpper emoji `shouldEqual` emoji
  -- for testing rules that only apply to the full algorithm
  let
    justFull lower upper = do
      toUpperSimple lower `shouldEqual` lower
      toUpper lower `shouldEqual` upper
  it "should map eszett correctly (ß -> SS)" do
    "ß" `justFull` "SS"
  it "should map ligatures (ﬀ, ﬄ) correctly" do
    "ﬀ" `justFull` "FF"
    "ﬄ" `justFull` "FFL"
    -- ARMENIAN SMALL LIGATURE MEN INI
    "\xFB15" `justFull` "\x0544\x053B"
  it "should handle precomposed characters without uppercase equivalents" do
    -- LATIN SMALL LETTER J WITH CARON
    "\x01F0" `justFull` "\x004A\x030C"
  it "should handle Greek characters with iota subscript" do
    -- GREEK SMALL LETTER UPSILON WITH PSILI AND VARIA
    "\x1F52" `justFull` "\x03A5\x0313\x0300"
    -- GREEK SMALL LETTER ALPHA WITH PSILI AND YPOGEGRAMMENI
    toUpperSimple "\x1F80" `shouldEqual` "\x1F88"
    toUpper "\x1F80" `shouldEqual` "\x1F08\x0399"
  it "should have no context sensitive matches" do
    -- Lithuanian retains the dot in a lowercase i when followed by accents.
    toUpper "i\x0307" `shouldEqual` "I\x0307"
    -- When uppercasing, i turns into a dotted capital I
    toUpper "i" `shouldEqual` "I"

toLowerTests :: forall m. MonadReader Int m => MonadEffect m => m Unit
toLowerTests = describe "toLower" do
  let accente = "\xE9"
  let accentE = "\xC9"
  it "should handle precomposed E with accent \xC9 -> \xE9" do
    toLowerSimple accentE `shouldEqual` accente
    toLower accentE `shouldEqual` accente
  it "should not affect emoji" do
    let emoji = "😃🧘🏻‍♂️🌍🍞🚗📞🎉♥️🏁"
    toLowerSimple emoji `shouldEqual` emoji
    toLower emoji `shouldEqual` emoji
  let dot = "\x0307"
  let dotI = "\x0130" -- composed
  it "should handle precomposed and decomposed I with dot" do
    toLowerSimple ("I" <> dot) `shouldEqual` ("i" <> dot) -- decomposed
    toLower ("I" <> dot) `shouldEqual` ("i" <> dot) -- decomposed
    toLowerSimple dotI `shouldEqual` "i" -- composed -> ASCII i
    toLower dotI `shouldEqual` ("i" <> dot) -- composed -> decomposed
  it "should handle Greek characters with iota subscript" do
    -- GREEK CAPITAL LETTER ALPHA WITH PSILI AND PROSGEGRAMMENI
    toLowerSimple "\x1F88" `shouldEqual` "\x1F80"
    toLower "\x1F88" `shouldEqual` "\x1F80"
  it "should have no context sensitive matches" do
    -- GREEK CAPITAL LETTER SIGMA
    toLower "\x03A3" `shouldEqual` "\x03C3"

toTitleTests :: forall m. MonadReader Int m => MonadEffect m => m Unit
toTitleTests = pure unit

hexDigitToIntTests :: forall m. MonadReader Int m => MonadEffect m => m Unit
hexDigitToIntTests = describe "hexDigitToInt" do
  it "'0'..'9' get mapped correctly" $
    map (hexDigitToInt <<< codePointFromChar) [ '0', '1', '2', '3', '4', '5', '6', '7', '8', '9' ] `shouldEqual`
      [ Just 0, Just 1, Just 2, Just 3, Just 4, Just 5, Just 6, Just 7, Just 8, Just 9 ]
  it "'a'..'f' get mapped correctly" $
    map (hexDigitToInt <<< codePointFromChar) [ 'a', 'b', 'c', 'd', 'e', 'f' ] `shouldEqual`
      [ Just 10, Just 11, Just 12, Just 13, Just 14, Just 15 ]
  it "'A'..'F' get mapped correctly" $
    map (hexDigitToInt <<< codePointFromChar) [ 'A', 'B', 'C', 'D', 'E', 'F' ] `shouldEqual`
      [ Just 10, Just 11, Just 12, Just 13, Just 14, Just 15 ]
  it "'G' is not a digit" $
    hexDigitToInt (codePointFromChar 'G') `shouldEqual` Nothing
  it "'♥' is not a digit" $
    hexDigitToInt (codePointFromChar '♥') `shouldEqual` Nothing
  it "'国' is not a digit" $
    hexDigitToInt (codePointFromChar '国') `shouldEqual` Nothing

isLetterTests :: forall m. MonadReader Int m => MonadEffect m => m Unit
isLetterTests = describe "isLetter" do
  it "isLetter == isAlpha" $ liftEffect $ quickCheck \(CP char) -> isLetter char == isAlpha char

isMarkTests :: forall m. MonadReader Int m => MonadEffect m => m Unit
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

isNumberTests :: forall m. MonadReader Int m => MonadEffect m => m Unit
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
  it "0..9 are Number" $ liftEffect $ quickCheck \(AsciiDecDigit char) -> isNumber char

isSeparatorTests :: forall m. MonadReader Int m => MonadEffect m => m Unit
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
  it "'\\xA0' is Separator" $
    isSeparator (codePointFromChar '\xA0') `shouldEqual` true
