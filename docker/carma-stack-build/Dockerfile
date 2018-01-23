# We use this just for Haskell Stack tool, the actual Stackage
# snapshot used is still the one in stack.yaml
FROM fpco/stack-build:lts-9.8

ARG BUILD_DATE
ARG VCS_REF

LABEL org.label-schema.build-date=$BUILD_DATE \
      org.label-schema.vcs-url="https://github.com/f-me/carma/tree/master/docker/carma-stack-build" \
      org.label-schema.vcs-ref=$VCS_REF \
      org.label-schema.schema-version="1.0.0-rc1"

ARG DOCKER_VERSION="17.03.0-ce"
ARG STACK_VERSION="1.6.3"
RUN curl -L -o /tmp/docker-$DOCKER_VERSION.tgz https://get.docker.com/builds/Linux/x86_64/docker-$DOCKER_VERSION.tgz && \
    tar -xz -C /tmp -f /tmp/docker-$DOCKER_VERSION.tgz && \
    mv /tmp/docker/* /usr/bin && \
    stack upgrade --binary-version $STACK_VERSION
