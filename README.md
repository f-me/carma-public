# CaRMa

[![Circle CI](https://circleci.com/gh/f-me/carma.svg?style=svg&circle-token=ed097e1dbbde9591b7b2bec9ce252ddc840deb54)](https://circleci.com/gh/f-me/carma)

## Building

Refer to `circle.yml` for full building instructions.

### Backend (Haskell)

#### macOS

On macOS with `openssl` installed via Homebrew, build with

    stack build --extra-include-dirs=/usr/local/opt/openssl/include/ --extra-include-dirs=/usr/local/opt/icu4c/include/ --extra-lib-dirs=/usr/local/opt/openssl/lib/ --extra-lib-dirs=/usr/local/opt/icu4c/lib/

#### Docker

    stack docker pull
    stack --docker image container

If the container has no access to your SSH keys (which may be the case
when using a token), use `stack dot` prior to building the image to
quickly fetch dependencies (`.stack-work/downloaded` directory is
shared between the container and the host anyways).

### Frontend (JS)

To build the client, do `cd resources/assets/react-components && npm
install && cd -` (just once), then `grunt build` to rebuild all client
code. Run `grunt watch` in background to rebuild client code
automatically when sources change.

## Running

1. Get a recent database snapshot. Backup folder with daily production
   snapshots is mounted on `carma-test` server at
   `/var/backups/allbackups/postgresql_carma`:

   ```
   scp <YOUR_LOGIN>@192.168.10.13:/var/backups/allbackups/postgresql_carma/2017-05-29_03-15_carma.sql.gz .
   ```
