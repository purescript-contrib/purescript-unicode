{ name = "unicode"
, license = "BSD-3-Clause"
, repository = "https://github.com/purescript-contrib/purescript-unicode"
, dependencies =
  [ "arrays"
  , "assert"
  , "console"
  , "effect"
  , "enums"
  , "foldable-traversable"
  , "integers"
  , "maybe"
  , "partial"
  , "prelude"
  , "psci-support"
  , "quickcheck"
  , "random"
  , "strings"
  , "transformers"
  , "unsafe-coerce"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
