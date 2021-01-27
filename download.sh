#!/bin/bash
VERSION=$(cat unicode-version)
for F in UnicodeData SpecialCasing CaseFolding; do
  wget "https://www.unicode.org/Public/$VERSION/ucd/$F.txt" -O $F.txt
done
