#!/bin/bash

set -eu

f1=data/day${1}test.txt
f2=data/day${1}test2.txt
f3=data/day${1}test3.txt
f4=data/day${1}test4.txt
f5=data/day${1}test5.txt
f6=data/day${1}test6.txt
f=data/day${1}.txt

cabal build frisby-advent-of-code2023

test -f $f1 && { echo; echo $f1; cat $f1 | cabal run frisby-advent-of-code2023 $1; }
test -f $f2 && { echo; echo $f2; cat $f2 | cabal run frisby-advent-of-code2023 $1; }
test -f $f3 && { echo; echo $f3; cat $f3 | cabal run frisby-advent-of-code2023 $1; }
test -f $f4 && { echo; echo $f4; cat $f4 | cabal run frisby-advent-of-code2023 $1; }
test -f $f5 && { echo; echo $f5; cat $f5 | cabal run frisby-advent-of-code2023 $1; }
test -f $f6 && { echo; echo $f6; cat $f6 | cabal run frisby-advent-of-code2023 $1; }

test -f $f && { echo; echo $f; cat $f | cabal run frisby-advent-of-code2023 $1; }
