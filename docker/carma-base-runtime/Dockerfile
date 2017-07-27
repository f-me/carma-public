FROM bitnami/minideb:jessie
RUN install_packages \
    ca-certificates \
    curl \
    libicu52 \
    libpq5 \
    xinetd \
    --

ARG BUILD_DATE
ARG VCS_REF

LABEL org.label-schema.build-date=$BUILD_DATE \
      org.label-schema.vcs-url="https://github.com/f-me/carma/tree/master/tools/baseimage-dockerfile" \
      org.label-schema.vcs-ref=$VCS_REF \
      org.label-schema.schema-version="1.0.0-rc1"

# https://www.fpcomplete.com/blog/2016/10/docker-demons-pid1-orphans-zombies-signals
ARG PID1_VERSION=0.1.0.1
RUN curl -L "https://github.com/fpco/pid1/releases/download/pid1%2F$PID1_VERSION/pid1-$PID1_VERSION-linux-x86_64.tar.gz" | tar xzf - -C /usr/local && \
    chown root:root /usr/local/sbin && \
    chown root:root /usr/local/sbin/pid1
ENTRYPOINT ["/usr/local/sbin/pid1"]
