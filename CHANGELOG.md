# Changelog

Notable changes to this project are documented in this file. The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/) and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

Breaking changes:

New features:

Bugfixes:

Other improvements:
- Run `isLowerTests` in tests (#35 by@JordanMartinez)

## [v5.0.0](https://github.com/purescript-contrib/purescript-unicode/releases/tag/v5.0.0) - 2021-02-26

Breaking changes:
- Added support for PureScript 0.14 and dropped support for all previous versions (#30 by @JordanMartinez)
- Main module renamed to `Data.CodePoint.Unicode`, functions now operate on `CodePoint`s, no longer `Char`s (#15 by @MonoidMusician)
- Simple case conversions renamed `toUpper` -> `toUpperSimple`, etc. (#15 by @MonoidMusician)
- Deprecation warnings for `isDigit` and `digitToInt` (#31 by @milesfrain)

New features:
- Added `hexDigitToInt`, `decDigitToInt`, `octDigitToInt` and deprecated `isDigit` and `digitToInt` (#31 by @milesfrain)
- New `toUpper`, `toLower`, `toTitle` based on full Unicode replacements, which may return more than one code point (#15 by @MonoidMusician)
- Added `caseFold` and `caseFoldSimple` (#15 by @MonoidMusician)
- New module `Data.String.Unicode` for case-conversion operating on strings (derived from the code point functions) (#15 by @MonoidMusician)
Bugfixes:

Other improvements:
- Changed default branch to `main` from `master`
- Updated to comply with Contributors library guidelines by adding new issue and pull request templates, updating documentation, and migrating to Spago for local development and CI (#22, #26, #29)

## [v4.0.1](https://github.com/purescript-contrib/purescript-unicode/releases/tag/v4.0.1) - 2018-06-23

- Added metadata including contributor guidelines and pushed latest release to Pursuit

## [v4.0.0](https://github.com/purescript-contrib/purescript-unicode/releases/tag/v4.0.0) - 2018-06-02

- Updated for PureScript 0.12

## [v3.0.2](https://github.com/purescript-contrib/purescript-unicode/releases/tag/v3.0.2) - 2017-11-11

- Improved performance

## [v3.0.0](https://github.com/purescript-contrib/purescript-unicode/releases/tag/v3.0.0) - 2017-04-05

- Updated for PureScript 0.11

## [v2.0.0](https://github.com/purescript-contrib/purescript-unicode/releases/tag/v2.0.0) - 2016-10-25

- Updated for PureScript 0.10

## [v1.0.0](https://github.com/purescript-contrib/purescript-unicode/releases/tag/v1.0.0) - 2016-06-06

- Initial release
