# carma-stack-build

[![](https://images.microbadger.com/badges/version/formalmethods/carma-stack-build.svg)](https://hub.docker.com/r/formalmethods/carma-stack-build)
[![](https://images.microbadger.com/badges/image/formalmethods/carma-stack-build.svg)](https://microbadger.com/images/formalmethods/carma-stack-build)

An image for building Dockerized Haskell apps, intended to be used
with Haskell Stack on CircleCI. It's based on `fpco/stack-build`, but
also contains Docker. Refer to
[`.circleci/config.yml`](https://github.com/ruamk/carma/blob/master/.circleci/config.yml)
for usage example.
