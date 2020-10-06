# Unicode Documentation

This directory contains documentation for `unicode`. If you are interested in contributing new documentation, please read the [contributor guidelines](../CONTRIBUTING.md) and [What Nobody Tells You About Documentation](https://documentation.divio.com) for help getting started.

## Generating Internal Modules

The [Data.Char.Unicode.Internal](src/Data/Char/Unicode/Internal.purs) module can be generated with the following command:

```sh
$ wget 'http://www.unicode.org/Public/6.0.0/ucd/UnicodeData.txt'
$ ./ubconfc < UnicodeData.txt > src/Data/Char/Unicode/Internal.purs
```