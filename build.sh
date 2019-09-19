#!/bin/sh

ghc -XMonadFailDesugaring -Wall -Wextra -Werror -Wincomplete-uni-patterns -isrc --make src/Main.hs -o Main
