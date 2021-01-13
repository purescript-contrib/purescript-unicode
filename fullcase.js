#!/usr/bin/env node

const fs = require("fs");

const SpecialCasing = ""+fs.readFileSync("SpecialCasing.txt");
const CaseFolding = ""+fs.readFileSync("CaseFolding.txt");

const special =
  /^([0-9A-F]+);\s+([0-9A-F]+(?:\s+[0-9A-F]+)*)?;\s+([0-9A-F]+(?:\s+[0-9A-F]+)*)?;\s+([0-9A-F]+(?:\s+[0-9A-F]+)*)?;\s+(#.+?)?$/mg;

const folding =
  /^([0-9A-F]+);\s+([CFS])?;\s+([0-9A-F]+(?:\s+[0-9A-F]+)*)?;\s+(#.+?)?$/mg;

const scData = {};
const cfData = {};

const rhex = a => typeof a === 'string' ? +('0x'+a) : +a;
const sortHex = (a,b) => rhex(a) - rhex(b);

for (const spec of SpecialCasing.matchAll(special)) {
  if (scData[spec[1]]) die("Duplicate special case rule");
  scData[spec[1]] = spec;
}
for (const fold of CaseFolding.matchAll(folding)) {
  if (cfData[fold[1]]) {
    cfData[fold[1]][fold[2]] = fold;
  } else {
    cfData[fold[1]] = { [fold[2]]: fold };
  }
}

const scKeys = Object.keys(scData).sort(sortHex);
const cfKeys = Object.keys(cfData).sort(sortHex);
const keys = scKeys.concat(cfKeys).sort(sortHex);

const data = {};

const hex = a => a ? '0x'+a : '0';
const hexes = a => a ? '[' + a.split(' ').filter(b=>b).map(hex) + ']' : '[]';

for (let code of keys) {
  const d = { code: hex(code) };
  const sc = scData[code];
  d.lower = hexes(sc?.[2]);
  d.title = hexes(sc?.[3]);
  d.upper = hexes(sc?.[4]);
  const cf = cfData[code];
  d.fold = hex(cf?.S?.[3] || cf?.C?.[3]);
  d.foldFull = hexes(cf?.F?.[3] || cf?.C?.[3]);
  data[code] = d;
}

const lines = keys.map(code => data[code]).map(d =>
  `{ code: ${d.code}, lower: ${d.lower}, title: ${d.title}, upper: ${d.upper}, fold: ${d.fold}, foldFull: ${d.foldFull} }`
);

const file = `module Data.CodePoint.Unicode.Casing where

import Prelude

import Data.Array as Array
import Data.CodePoint.Unicode.Internal (bsearch, uTowlower, uTowtitle, uTowupper)
import Data.Maybe (Maybe(..))

type CaseRec =
  {
    code :: Int,
    lower :: Array Int,
    title :: Array Int,
    upper :: Array Int,
    fold :: Int,
    foldFull :: Array Int
  }

rules :: Array CaseRec
rules = [
  ${lines.join(",\n  ")}
]

zeroRec :: Int -> CaseRec
zeroRec code = { code, lower: [], title: [], upper: [], fold: 0, foldFull: [] }

recCmp :: CaseRec -> CaseRec -> Ordering
recCmp { code } { code: code' } = compare code code'

findRule :: Int -> CaseRec
findRule code = case bsearch (zeroRec code) rules (Array.length rules) recCmp of
  Nothing -> zeroRec code
  Just r -> r

fold :: Int -> Int
fold code =
  let folded = (findRule code).fold
  in if folded == 0 then uTowlower code else folded

foldFull :: Int -> Array Int
foldFull code =
  let folded = (findRule code).foldFull
  in if Array.null folded then [uTowlower code] else folded

lower :: Int -> Array Int
lower code =
  let lowered = (findRule code).lower
  in if Array.null lowered then [uTowlower code] else lowered

title :: Int -> Array Int
title code =
  let titled = (findRule code).title
  in if Array.null titled then [uTowtitle code] else titled

upper :: Int -> Array Int
upper code =
  let uppered = (findRule code).upper
  in if Array.null uppered then [uTowupper code] else uppered
`;

fs.writeFileSync("src/Data/CodePoint/Unicode/Casing.purs", file);
