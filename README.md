# CaRMa

[![Circle CI](https://circleci.com/gh/f-me/carma.svg?style=svg&circle-token=ed097e1dbbde9591b7b2bec9ce252ddc840deb54)][ci]

## Building

Refer to [`.circleci/config.yml`](.circleci/config.yml) for full building instructions.

### Backend (Haskell)

#### Generic

1. Install [Haskell Stack][haskell-stack]. Check that it works:

   ```
   stack --numeric-version
   1.4.0
   ```

2. Install PostgreSQL 9.3 (this is what production servers use) and
   PostGIS. Create a database named `carma`:

   ```
   createdb carma
   ```

3. Add roles:

   ```
   createuser -s carma
   createuser carma_db_sync
   createuser carma_geo
   createuser carma_sms
   createuser mail_svc
   createuser fmuser
   createuser pavel.golovnin
   createuser reportgen
   ```

   `carma` is the superuser which owns the database and is used to run
   DB migration scripts.

3. Unpack and recover a database snapshot (note that this needs to run
   as a PostgreSQL superuser - for example, prepend with `sudo -u postgres`):

   ```
   psql carma -f 2017-05-29_03-15_carma.sql
   ```

3. Set passwords:

   ```
   psql carma -c "alter user carma with password 'pass'"
   psql carma -c "alter user carma_db_sync with password 'pass'"
   ```

3. Clone CaRMa Git repository:

   ```
   git clone git@github.com:f-me/carma.git
   cd carma
   ```

3. Add log files:

   ```
   mkdir srv/log && > srv/log/access.log && > srv/log/error.log
   ```

4. Build the backend:

   ```
   stack --install-ghc install
   ```

5. Change the current directory to `srv/` and run the server:

    ```
    cd srv
    carma -p 8000
    ```

6. Check that the server works:

   ```
   curl localhost:8000/meta
   ```

#### macOS

On macOS with `openssl` and `icu4c` installed via Homebrew, build with

    stack build --extra-include-dirs=/usr/local/opt/openssl/include/ --extra-include-dirs=/usr/local/opt/icu4c/include/ --extra-lib-dirs=/usr/local/opt/openssl/lib/ --extra-lib-dirs=/usr/local/opt/icu4c/lib/

#### Docker

Alternatively, to build the server inside Docker and package all
executables in a container:

    stack docker pull
    stack --docker image container

If you're on Linux, you can just build with `stack --docker build` and
run the app outside Docker. You will not be able to run the app built
this way on macOS or Windows.

If the container has no access to your SSH keys (which may be the case
when using a token), use `stack dot` prior to building the image to
quickly fetch dependencies (`.stack-work/downloaded` directory is
shared between the container and the host anyways).

Note that this container lacks frontend resources necessary to launch
CaRMa - see below for how to build the whole bundle.

### Frontend (JS)

To build the client from scratch:

    cd srv
    npm install -g bower grunt && npm install && bower install
    cd resources/assets/react-components && npm install && cd -
    mkdir -p resources/static/{js/gen,css}
    grunt build

To rebuild the client:

    grunt build

Run `grunt watch` in background to rebuild client code automatically
when sources change.

### Docker bundle

If you built the frontend and the backend Docker image, you can now
build an image containing both:

    cd srv/
    docker build -t carma-bundle .

CaRMa server executable inside the container is located at
`/usr/local/bin/carma`. The command to run the container is

    docker run carma-bundle /usr/local/bin/carma

### CI build

[CircleCI][ci] is configured to build CaRMa on every push. If you have
Docker installed, you may use [CircleCI CLI tool][ci-cli] to performs
builds in the same environment as on CI server:

    circleci build --job build_client
    circleci build --job build_server

Jobs run in an auto-removed container, so at the moment you may
only use them to test that the build finishes successfully.

## Running

1. Get a recent database snapshot. Backup folder with daily production
   snapshots is mounted on `carma-test` server at
   `/var/backups/allbackups/postgresql_carma`:

   ```
   scp <YOUR_LOGIN>@192.168.10.13:/var/backups/allbackups/postgresql_carma/2017-05-29_03-15_carma.sql.gz .
   ```

[ci]: https://circleci.com/gh/f-me/carma
[ci-cli]: https://circleci.com/docs/2.0/local-jobs/#installing-the-cli-locally
[haskell-stack]: https://docs.haskellstack.org/en/stable/README/
