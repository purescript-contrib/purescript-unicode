# Contributing to Unicode

Thanks for your interest in contributing to `unicode`! We welcome new contributions regardless of your level of experience or familiarity with PureScript.

Every library in the Contributors organization shares a simple handbook that helps new contributors get started. With that in mind, please [read the short contributing guide on purescript-contrib/governance](https://github.com/purescript-contrib/governance/blob/main/contributing.md) before contributing to this library.

## Generating Internal Modules

The [Data.CodePoint.Unicode.Internal](../src/Data/CodePoint/Unicode/Internal.purs) and [Data.CodePoint.Unicode.Internal.Casing](../src/Data/CodePoint/Unicode/Internal/Casing.purs) modules can be generated with the following command from the root of this repository:

```sh
$ ./download.sh
$ ./ubconfc < UnicodeData.txt > src/Data/CodePoint/Unicode/Internal.purs
$ ./fullcase.js
$ purs-tidy format-in-place src/Data/CodePoint/Unicode/Internal.purs src/Data/CodePoint/Unicode/Internal/Casing.purs
```

(Note that this downloads data according to the version stored in the [`unicode-version`](../unicode-version) file in the root.)
