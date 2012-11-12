#!/bin/sh

runhaskell -package-conf=cabal-dev/packages-7.6.1.conf src/tasks/$1.hs
