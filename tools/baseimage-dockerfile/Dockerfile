FROM bitnami/minideb:jessie
RUN install_packages \
    ca-certificates \
    libicu52 \
    libpq5 \
    wget \
    xinetd \
    --

# https://www.fpcomplete.com/blog/2016/10/docker-demons-pid1-orphans-zombies-signals
ARG PID1_VERSION=0.1.0.1
RUN wget -O- "https://github.com/fpco/pid1/releases/download/pid1%2F$PID1_VERSION/pid1-$PID1_VERSION-linux-x86_64.tar.gz" | tar xzf - -C /usr/local && \
    chown root:root /usr/local/sbin && \
    chown root:root /usr/local/sbin/pid1
ENTRYPOINT ["/usr/local/sbin/pid1"]
