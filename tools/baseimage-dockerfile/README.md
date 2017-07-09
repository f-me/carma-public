# haskell-run

[![](https://images.microbadger.com/badges/version/formalmethods/haskell-run.svg)](https://hub.docker.com/r/formalmethods/haskell-run)
[![](https://images.microbadger.com/badges/image/formalmethods/haskell-run.svg)](https://microbadger.com/images/formalmethods/haskell-run)

A base image for Dockerized Haskell apps, intended to be used with
Haskell Stack. Currently it only contains the minimum amount of
libraries required to run CaRMa.

To use, put in your `stack.yaml` the following:

    image:
      containers:
        - base: "formalmethods/haskell-run"

Now build your project container with `stack image container`.
