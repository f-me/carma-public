#!/bin/bash

if [[ ! -d deps ]]; then
  mkdir deps
  cd deps
  git clone https://github.com/snapframework/snap.git
  git clone https://github.com/snapframework/snap-core.git
  git clone https://github.com/snapframework/snap-server.git
  git clone https://github.com/snapframework/heist.git
  git clone https://github.com/dzhus/snaplet-redis.git
  git clone https://github.com/f-me/snaplet-redson.git
  cd ..
fi

cd srv
cabal-dev clean
cabal-dev add-source ../deps/snap
cabal-dev add-source ../deps/snap-core
cabal-dev add-source ../deps/snap-server
cabal-dev add-source ../deps/heist
cabal-dev add-source ../deps/snaplet-redis
cabal-dev add-source ../deps/snaplet-redson

cabal-dev install --reinstall snap snap-core snap-server heist
cabal-dev install --reinstall snaplet-redis
cabal-dev install --reinstall snaplet-redson
cabal-dev configure && cabal-dev build

cd ../tools/legacy-data-loader

cabal-dev clean
cabal-dev add-source ../../deps/snap
cabal-dev add-source ../../deps/snap-core
cabal-dev add-source ../../deps/snap-server
cabal-dev add-source ../../deps/heist
cabal-dev add-source ../../deps/snaplet-redis
cabal-dev add-source ../../deps/snaplet-redson

cabal-dev install --reinstall snap-core heist
cabal-dev install --reinstall snaplet-redis
cabal-dev install --reinstall snaplet-redson
cabal-dev configure && cabal-dev build

cd $OLDPWD
