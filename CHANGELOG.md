# Changelog

Notable changes to this project are documented in this file. The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/) and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

Breaking changes (😱!!!):
- Main module renamed to `Data.CodePoint.Unicode`, functions now operate on `CodePoint`s, no longer `Char`s (#15 by @MonoidMusician)
- Simple case conversions renamed `toUpper` -> `toUpperSimple`, etc. (#15 by @MonoidMusician)
- Deprecation warnings for `isDigit` and `digitToInt` (#31 by @milesfrain)

New features:
- Added `hexDigitToInt` `decDigitToInt` `octDigitToInt` (#31 by @milesfrain)
- New `toUpper`, `toLower`, `toTitle` based on full Unicode replacements, which may return more than one code point (#15 by @MonoidMusician)
- Added `caseFold` and `caseFoldSimple` (#15 by @MonoidMusician)
- New module `Data.String.Unicode` for case-conversion operating on strings (derived from the code point functions) (#15 by @MonoidMusician)

Bugfixes:

Other improvements:

## [v4.0.1](https://github.com/purescript-contrib/purescript-unicode/releases/tag/v4.0.1) - 2018-06-23

- Adds metadata including contributor guidelines
- Pushes latest release to Pursuit

## [v4.0.0](https://github.com/purescript-contrib/purescript-unicode/releases/tag/v4.0.0) - 2018-06-02

- Updates for 0.12

## [v3.0.2](https://github.com/purescript-contrib/purescript-unicode/releases/tag/v3.0.2) - 2017-11-11

- Performance improvements
