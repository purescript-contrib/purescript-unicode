# purescript-unicode

[![Latest release](http://img.shields.io/bower/v/purescript-unicode.svg)](https://github.com/purescript-contrib/purescript-unicode/releases)
[![Build Status](https://travis-ci.org/purescript-contrib/purescript-unicode.svg?branch=master)](https://travis-ci.org/purescript-contrib/purescript-unicode)
[![Maintainer: cdepillabout](https://img.shields.io/badge/maintainer-cdepillabout-lightgrey.svg)](http://github.com/cdepillabout)

Unicode character functions.

## Installation

```sh
$ bower install purescript-unicode
```

## Module documentation

- [Data.CodePoint.Unicode](docs/Data/Char/Unicode.md)

## Generate Internal module

The [Data.CodePoint.Unicode.Internal](src/Data/Char/Unicode/Internal.purs) module
can be generated with the following command:

```sh
$ wget 'http://www.unicode.org/Public/6.0.0/ucd/UnicodeData.txt'
$ ./ubconfc < UnicodeData.txt > src/Data/CodePoint/Unicode/Internal.purs
```
