#!/bin/sh

ghc -Wall -Wextra -Werror -Wincomplete-uni-patterns -isrc --make src/Main.hs -o Main
