# Dockerfile to build CaRMa bundle (backend + frontend). See README.md
# for instructions.
#
# Base image is built using `stack image container`
FROM carma:latest
RUN mkdir -p /srv/resources && \
    mkdir /srv/log/
# JS code
COPY resources /srv/resources
WORKDIR /srv/
CMD ["/usr/local/bin/carma"]
