# Unicode Documentation

This directory contains documentation for `unicode`. If you are interested in contributing new documentation, please read the [contributor guidelines](../CONTRIBUTING.md) and [What Nobody Tells You About Documentation](https://documentation.divio.com) for help getting started.

## Generating Internal Modules

The [Data.CodePoint.Unicode.Internal](../src/Data/CodePoint/Unicode/Internal.purs) and [Data.CodePoint.Unicode.Internal.Casing](../src/Data/CodePoint/Unicode/Internal/Casing.purs) modules can be generated with the following command from the root of this repository:

```sh
$ ./download.sh
$ ./ubconfc < UnicodeData.txt > src/Data/CodePoint/Unicode/Internal.purs
$ ./fullcase.js
$ purs-tidy format-in-place src/Data/CodePoint/Unicode/Internal.purs src/Data/CodePoint/Unicode/Internal/Casing.purs
```

(Note that this downloads data according to the version stored in the [`unicode-version`](../unicode-version) file in the root.)
