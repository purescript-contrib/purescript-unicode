## Module Data.CodePoint.Unicode.Internal

#### `UnicodeCategory`

``` purescript
data UnicodeCategory
  = NUMCAT_LU
  | NUMCAT_LL
  | NUMCAT_LT
  | NUMCAT_LM
  | NUMCAT_LO
  | NUMCAT_MN
  | NUMCAT_MC
  | NUMCAT_ME
  | NUMCAT_ND
  | NUMCAT_NL
  | NUMCAT_NO
  | NUMCAT_PC
  | NUMCAT_PD
  | NUMCAT_PS
  | NUMCAT_PE
  | NUMCAT_PI
  | NUMCAT_PF
  | NUMCAT_PO
  | NUMCAT_SM
  | NUMCAT_SC
  | NUMCAT_SK
  | NUMCAT_SO
  | NUMCAT_ZS
  | NUMCAT_ZL
  | NUMCAT_ZP
  | NUMCAT_CC
  | NUMCAT_CF
  | NUMCAT_CS
  | NUMCAT_CO
  | NUMCAT_CN
```

##### Instances
``` purescript
Show UnicodeCategory
```

#### `ConversionRule`

``` purescript
newtype ConversionRule
  = ConversionRule { category :: Int, unicodeCat :: UnicodeCategory, possible :: Int, updist :: Int, lowdist :: Int, titledist :: Int }
```

##### Instances
``` purescript
Show ConversionRule
```

#### `CharBlock`

``` purescript
newtype CharBlock
  = CharBlock { start :: Int, length :: Int, convRule :: ConversionRule }
```

##### Instances
``` purescript
Show CharBlock
```

#### `gencatPF`

``` purescript
gencatPF :: Int
```

#### `gencatSM`

``` purescript
gencatSM :: Int
```

#### `gencatSO`

``` purescript
gencatSO :: Int
```

#### `gencatPI`

``` purescript
gencatPI :: Int
```

#### `gencatMC`

``` purescript
gencatMC :: Int
```

#### `gencatCO`

``` purescript
gencatCO :: Int
```

#### `gencatME`

``` purescript
gencatME :: Int
```

#### `gencatPO`

``` purescript
gencatPO :: Int
```

#### `gencatCS`

``` purescript
gencatCS :: Int
```

#### `gencatPS`

``` purescript
gencatPS :: Int
```

#### `gencatMN`

``` purescript
gencatMN :: Int
```

#### `gencatZL`

``` purescript
gencatZL :: Int
```

#### `gencatZP`

``` purescript
gencatZP :: Int
```

#### `gencatZS`

``` purescript
gencatZS :: Int
```

#### `gencatLL`

``` purescript
gencatLL :: Int
```

#### `gencatLM`

``` purescript
gencatLM :: Int
```

#### `gencatLO`

``` purescript
gencatLO :: Int
```

#### `gencatND`

``` purescript
gencatND :: Int
```

#### `gencatLT`

``` purescript
gencatLT :: Int
```

#### `gencatSC`

``` purescript
gencatSC :: Int
```

#### `gencatLU`

``` purescript
gencatLU :: Int
```

#### `gencatNL`

``` purescript
gencatNL :: Int
```

#### `gencatCC`

``` purescript
gencatCC :: Int
```

#### `gencatNO`

``` purescript
gencatNO :: Int
```

#### `gencatCF`

``` purescript
gencatCF :: Int
```

#### `gencatPC`

``` purescript
gencatPC :: Int
```

#### `gencatSK`

``` purescript
gencatSK :: Int
```

#### `gencatPD`

``` purescript
gencatPD :: Int
```

#### `gencatPE`

``` purescript
gencatPE :: Int
```

#### `maxUniChar`

``` purescript
maxUniChar :: Int
```

#### `numBlocks`

``` purescript
numBlocks :: Int
```

#### `numConvBlocks`

``` purescript
numConvBlocks :: Int
```

#### `numSpaceBlocks`

``` purescript
numSpaceBlocks :: Int
```

#### `numLat1Blocks`

``` purescript
numLat1Blocks :: Int
```

#### `numRules`

``` purescript
numRules :: Int
```

#### `rule165`

``` purescript
rule165 :: ConversionRule
```

#### `rule63`

``` purescript
rule63 :: ConversionRule
```

#### `rule126`

``` purescript
rule126 :: ConversionRule
```

#### `rule162`

``` purescript
rule162 :: ConversionRule
```

#### `rule71`

``` purescript
rule71 :: ConversionRule
```

#### `rule21`

``` purescript
rule21 :: ConversionRule
```

#### `rule128`

``` purescript
rule128 :: ConversionRule
```

#### `rule44`

``` purescript
rule44 :: ConversionRule
```

#### `rule161`

``` purescript
rule161 :: ConversionRule
```

#### `rule105`

``` purescript
rule105 :: ConversionRule
```

#### `rule101`

``` purescript
rule101 :: ConversionRule
```

#### `rule43`

``` purescript
rule43 :: ConversionRule
```

#### `rule77`

``` purescript
rule77 :: ConversionRule
```

#### `rule143`

``` purescript
rule143 :: ConversionRule
```

#### `rule39`

``` purescript
rule39 :: ConversionRule
```

#### `rule41`

``` purescript
rule41 :: ConversionRule
```

#### `rule72`

``` purescript
rule72 :: ConversionRule
```

#### `rule28`

``` purescript
rule28 :: ConversionRule
```

#### `rule31`

``` purescript
rule31 :: ConversionRule
```

#### `rule36`

``` purescript
rule36 :: ConversionRule
```

#### `rule102`

``` purescript
rule102 :: ConversionRule
```

#### `rule80`

``` purescript
rule80 :: ConversionRule
```

#### `rule5`

``` purescript
rule5 :: ConversionRule
```

#### `rule113`

``` purescript
rule113 :: ConversionRule
```

#### `rule142`

``` purescript
rule142 :: ConversionRule
```

#### `rule145`

``` purescript
rule145 :: ConversionRule
```

#### `rule104`

``` purescript
rule104 :: ConversionRule
```

#### `rule61`

``` purescript
rule61 :: ConversionRule
```

#### `rule93`

``` purescript
rule93 :: ConversionRule
```

#### `rule62`

``` purescript
rule62 :: ConversionRule
```

#### `rule103`

``` purescript
rule103 :: ConversionRule
```

#### `rule60`

``` purescript
rule60 :: ConversionRule
```

#### `rule96`

``` purescript
rule96 :: ConversionRule
```

#### `rule51`

``` purescript
rule51 :: ConversionRule
```

#### `rule87`

``` purescript
rule87 :: ConversionRule
```

#### `rule86`

``` purescript
rule86 :: ConversionRule
```

#### `rule123`

``` purescript
rule123 :: ConversionRule
```

#### `rule164`

``` purescript
rule164 :: ConversionRule
```

#### `rule166`

``` purescript
rule166 :: ConversionRule
```

#### `rule27`

``` purescript
rule27 :: ConversionRule
```

#### `rule158`

``` purescript
rule158 :: ConversionRule
```

#### `rule157`

``` purescript
rule157 :: ConversionRule
```

#### `rule9`

``` purescript
rule9 :: ConversionRule
```

#### `rule159`

``` purescript
rule159 :: ConversionRule
```

#### `rule138`

``` purescript
rule138 :: ConversionRule
```

#### `rule4`

``` purescript
rule4 :: ConversionRule
```

#### `rule133`

``` purescript
rule133 :: ConversionRule
```

#### `rule155`

``` purescript
rule155 :: ConversionRule
```

#### `rule132`

``` purescript
rule132 :: ConversionRule
```

#### `rule153`

``` purescript
rule153 :: ConversionRule
```

#### `rule24`

``` purescript
rule24 :: ConversionRule
```

#### `rule26`

``` purescript
rule26 :: ConversionRule
```

#### `rule16`

``` purescript
rule16 :: ConversionRule
```

#### `rule74`

``` purescript
rule74 :: ConversionRule
```

#### `rule122`

``` purescript
rule122 :: ConversionRule
```

#### `rule25`

``` purescript
rule25 :: ConversionRule
```

#### `rule127`

``` purescript
rule127 :: ConversionRule
```

#### `rule40`

``` purescript
rule40 :: ConversionRule
```

#### `rule30`

``` purescript
rule30 :: ConversionRule
```

#### `rule29`

``` purescript
rule29 :: ConversionRule
```

#### `rule69`

``` purescript
rule69 :: ConversionRule
```

#### `rule3`

``` purescript
rule3 :: ConversionRule
```

#### `rule135`

``` purescript
rule135 :: ConversionRule
```

#### `rule23`

``` purescript
rule23 :: ConversionRule
```

#### `rule32`

``` purescript
rule32 :: ConversionRule
```

#### `rule33`

``` purescript
rule33 :: ConversionRule
```

#### `rule50`

``` purescript
rule50 :: ConversionRule
```

#### `rule154`

``` purescript
rule154 :: ConversionRule
```

#### `rule58`

``` purescript
rule58 :: ConversionRule
```

#### `rule67`

``` purescript
rule67 :: ConversionRule
```

#### `rule76`

``` purescript
rule76 :: ConversionRule
```

#### `rule119`

``` purescript
rule119 :: ConversionRule
```

#### `rule57`

``` purescript
rule57 :: ConversionRule
```

#### `rule97`

``` purescript
rule97 :: ConversionRule
```

#### `rule108`

``` purescript
rule108 :: ConversionRule
```

#### `rule100`

``` purescript
rule100 :: ConversionRule
```

#### `rule70`

``` purescript
rule70 :: ConversionRule
```

#### `rule141`

``` purescript
rule141 :: ConversionRule
```

#### `rule139`

``` purescript
rule139 :: ConversionRule
```

#### `rule45`

``` purescript
rule45 :: ConversionRule
```

#### `rule91`

``` purescript
rule91 :: ConversionRule
```

#### `rule121`

``` purescript
rule121 :: ConversionRule
```

#### `rule117`

``` purescript
rule117 :: ConversionRule
```

#### `rule12`

``` purescript
rule12 :: ConversionRule
```

#### `rule85`

``` purescript
rule85 :: ConversionRule
```

#### `rule163`

``` purescript
rule163 :: ConversionRule
```

#### `rule17`

``` purescript
rule17 :: ConversionRule
```

#### `rule134`

``` purescript
rule134 :: ConversionRule
```

#### `rule147`

``` purescript
rule147 :: ConversionRule
```

#### `rule64`

``` purescript
rule64 :: ConversionRule
```

#### `rule2`

``` purescript
rule2 :: ConversionRule
```

#### `rule84`

``` purescript
rule84 :: ConversionRule
```

#### `rule38`

``` purescript
rule38 :: ConversionRule
```

#### `rule42`

``` purescript
rule42 :: ConversionRule
```

#### `rule53`

``` purescript
rule53 :: ConversionRule
```

#### `rule83`

``` purescript
rule83 :: ConversionRule
```

#### `rule98`

``` purescript
rule98 :: ConversionRule
```

#### `rule136`

``` purescript
rule136 :: ConversionRule
```

#### `rule120`

``` purescript
rule120 :: ConversionRule
```

#### `rule20`

``` purescript
rule20 :: ConversionRule
```

#### `rule115`

``` purescript
rule115 :: ConversionRule
```

#### `rule109`

``` purescript
rule109 :: ConversionRule
```

#### `rule13`

``` purescript
rule13 :: ConversionRule
```

#### `rule19`

``` purescript
rule19 :: ConversionRule
```

#### `rule125`

``` purescript
rule125 :: ConversionRule
```

#### `rule49`

``` purescript
rule49 :: ConversionRule
```

#### `rule79`

``` purescript
rule79 :: ConversionRule
```

#### `rule14`

``` purescript
rule14 :: ConversionRule
```

#### `rule148`

``` purescript
rule148 :: ConversionRule
```

#### `rule66`

``` purescript
rule66 :: ConversionRule
```

#### `rule99`

``` purescript
rule99 :: ConversionRule
```

#### `rule140`

``` purescript
rule140 :: ConversionRule
```

#### `rule116`

``` purescript
rule116 :: ConversionRule
```

#### `rule8`

``` purescript
rule8 :: ConversionRule
```

#### `rule94`

``` purescript
rule94 :: ConversionRule
```

#### `rule114`

``` purescript
rule114 :: ConversionRule
```

#### `rule6`

``` purescript
rule6 :: ConversionRule
```

#### `rule7`

``` purescript
rule7 :: ConversionRule
```

#### `rule55`

``` purescript
rule55 :: ConversionRule
```

#### `rule54`

``` purescript
rule54 :: ConversionRule
```

#### `rule124`

``` purescript
rule124 :: ConversionRule
```

#### `rule65`

``` purescript
rule65 :: ConversionRule
```

#### `rule78`

``` purescript
rule78 :: ConversionRule
```

#### `rule56`

``` purescript
rule56 :: ConversionRule
```

#### `rule137`

``` purescript
rule137 :: ConversionRule
```

#### `rule131`

``` purescript
rule131 :: ConversionRule
```

#### `rule130`

``` purescript
rule130 :: ConversionRule
```

#### `rule110`

``` purescript
rule110 :: ConversionRule
```

#### `rule48`

``` purescript
rule48 :: ConversionRule
```

#### `rule52`

``` purescript
rule52 :: ConversionRule
```

#### `rule156`

``` purescript
rule156 :: ConversionRule
```

#### `rule75`

``` purescript
rule75 :: ConversionRule
```

#### `rule11`

``` purescript
rule11 :: ConversionRule
```

#### `rule129`

``` purescript
rule129 :: ConversionRule
```

#### `rule37`

``` purescript
rule37 :: ConversionRule
```

#### `rule18`

``` purescript
rule18 :: ConversionRule
```

#### `rule152`

``` purescript
rule152 :: ConversionRule
```

#### `rule35`

``` purescript
rule35 :: ConversionRule
```

#### `rule46`

``` purescript
rule46 :: ConversionRule
```

#### `rule82`

``` purescript
rule82 :: ConversionRule
```

#### `rule10`

``` purescript
rule10 :: ConversionRule
```

#### `rule34`

``` purescript
rule34 :: ConversionRule
```

#### `rule150`

``` purescript
rule150 :: ConversionRule
```

#### `rule107`

``` purescript
rule107 :: ConversionRule
```

#### `rule47`

``` purescript
rule47 :: ConversionRule
```

#### `rule160`

``` purescript
rule160 :: ConversionRule
```

#### `rule73`

``` purescript
rule73 :: ConversionRule
```

#### `rule59`

``` purescript
rule59 :: ConversionRule
```

#### `rule106`

``` purescript
rule106 :: ConversionRule
```

#### `rule151`

``` purescript
rule151 :: ConversionRule
```

#### `rule15`

``` purescript
rule15 :: ConversionRule
```

#### `rule112`

``` purescript
rule112 :: ConversionRule
```

#### `rule90`

``` purescript
rule90 :: ConversionRule
```

#### `rule146`

``` purescript
rule146 :: ConversionRule
```

#### `rule89`

``` purescript
rule89 :: ConversionRule
```

#### `rule81`

``` purescript
rule81 :: ConversionRule
```

#### `rule88`

``` purescript
rule88 :: ConversionRule
```

#### `rule149`

``` purescript
rule149 :: ConversionRule
```

#### `rule111`

``` purescript
rule111 :: ConversionRule
```

#### `rule144`

``` purescript
rule144 :: ConversionRule
```

#### `rule92`

``` purescript
rule92 :: ConversionRule
```

#### `rule118`

``` purescript
rule118 :: ConversionRule
```

#### `rule22`

``` purescript
rule22 :: ConversionRule
```

#### `rule68`

``` purescript
rule68 :: ConversionRule
```

#### `rule95`

``` purescript
rule95 :: ConversionRule
```

#### `rule0`

``` purescript
rule0 :: ConversionRule
```

#### `rule1`

``` purescript
rule1 :: ConversionRule
```

#### `allchars`

``` purescript
allchars :: Array CharBlock
```

#### `convchars`

``` purescript
convchars :: Array CharBlock
```

#### `spacechars`

``` purescript
spacechars :: Array CharBlock
```

#### `nullrule`

``` purescript
nullrule :: ConversionRule
```

#### `blkCmp`

``` purescript
blkCmp :: CharBlock -> CharBlock -> Ordering
```

#### `getRule`

``` purescript
getRule :: Array CharBlock -> Int -> Int -> Maybe ConversionRule
```

#### `bsearch`

``` purescript
bsearch :: forall a. a -> Array a -> Int -> (a -> a -> Ordering) -> Maybe a
```

#### `checkAttr`

``` purescript
checkAttr :: Array Int -> Int -> Boolean
```

#### `checkAttrS`

``` purescript
checkAttrS :: Array Int -> Int -> Boolean
```

#### `uIswcntrl`

``` purescript
uIswcntrl :: Int -> Boolean
```

#### `uIswprint`

``` purescript
uIswprint :: Int -> Boolean
```

#### `uIswupper`

``` purescript
uIswupper :: Int -> Boolean
```

#### `uIswlower`

``` purescript
uIswlower :: Int -> Boolean
```

#### `uIswalpha`

``` purescript
uIswalpha :: Int -> Boolean
```

#### `uIswdigit`

``` purescript
uIswdigit :: Int -> Boolean
```

#### `uIswalnum`

``` purescript
uIswalnum :: Int -> Boolean
```

#### `uIswspace`

``` purescript
uIswspace :: Int -> Boolean
```

#### `caseConv`

``` purescript
caseConv :: (ConversionRule -> Int) -> Int -> Int
```

#### `uTowupper`

``` purescript
uTowupper :: Int -> Int
```

#### `uTowlower`

``` purescript
uTowlower :: Int -> Int
```

#### `uTowtitle`

``` purescript
uTowtitle :: Int -> Int
```

#### `uGencat`

``` purescript
uGencat :: Int -> Maybe UnicodeCategory
```


