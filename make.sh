#!/bin/bash

cd src

ghc --make -O2 FilterDictionary.hs

ghc --make -O2 Hangman.hs

rm *.o
rm *.hi

mv FilterDictionary Hangman ../bin
