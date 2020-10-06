{ name = "unicode"
, dependencies =
  [ "console"
  , "effect"
  , "foldable-traversable"
  , "maybe"
  , "psci-support"
  , "quickcheck"
  , "random"
  , "spec"
  , "strings"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
