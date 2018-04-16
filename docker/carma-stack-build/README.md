# carma-stack-build

[![](https://images.microbadger.com/badges/version/ruamc/carma-stack-build.svg)](https://hub.docker.com/r/ruamc/carma-stack-build)
[![](https://images.microbadger.com/badges/image/ruamc/carma-stack-build.svg)](https://microbadger.com/images/ruamc/carma-stack-build)

An image for building Dockerized Haskell apps, intended to be used
with Haskell Stack on CircleCI. It's based on `fpco/stack-build`, but
also contains Docker. Refer to
[`.circleci/config.yml`](https://github.com/ruamk/carma/blob/master/.circleci/config.yml)
for usage example.
