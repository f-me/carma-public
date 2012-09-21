#!/bin/env bash

runhaskell -package-conf=cabal-dev/packages-7.4.1.conf src/tasks/$1.hs
