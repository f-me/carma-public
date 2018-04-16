# carma-base-runtime

[![](https://images.microbadger.com/badges/version/ruamc/carma-base-runtime.svg)](https://hub.docker.com/r/ruamc/carma-base-runtime)
[![](https://images.microbadger.com/badges/image/ruamc/carma-base-runtime.svg)](https://microbadger.com/images/ruamc/carma-base-runtime)

A base image for Dockerized Haskell apps, intended to be used with
Haskell Stack. Currently it only contains the minimum amount of
libraries required to run CaRMa.

To use, put in your `stack.yaml` the following:

    image:
      containers:
        - base: "ruamc/carma-base-runtime"

Now build your project container with `stack image container`.
