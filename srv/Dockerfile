# Dockerfile to build CaRMa bundle (backend + frontend). See README.md
# for instructions.
#
# Base image is built using `stack image container`
FROM carma:latest
RUN mkdir -p /srv/resources && \
    mkdir /srv/log/

ARG BUILD_DATE
ARG VCS_REF

LABEL org.label-schema.build-date=$BUILD_DATE \
      org.label-schema.vcs-url="https://github.com/f-me/carma" \
      org.label-schema.vcs-ref=$VCS_REF \
      org.label-schema.schema-version="1.0.0-rc1"

# JS code
COPY resources /srv/resources
WORKDIR /srv/
CMD ["/usr/local/bin/carma"]
