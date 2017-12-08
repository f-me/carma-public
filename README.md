# CaRMa

[![Circle CI](https://circleci.com/gh/f-me/carma.svg?style=shield&circle-token=ed097e1dbbde9591b7b2bec9ce252ddc840deb54)][ci]
[![](https://images.microbadger.com/badges/image/formalmethods/carma-bundle.svg)](https://microbadger.com/images/formalmethods/carma-bundle)

## Building

Refer to [`.circleci/config.yml`][ci-config] for full building
instructions.

### Backend (Haskell)

#### Generic

1. Install [Haskell Stack][haskell-stack]. Check that it works:

   ```
   stack --numeric-version
   1.6.1
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

<strong>Warning!</strong> <em>If you're building front-end
after migrating to webpack, run `npm run clean-old-stuff` first.</em>

To build front-end from scratch (development build):

```bash
$ cd srv
$ npm install
$ npm run build
$ npm run build-backend-templates
```

If you need to just rebuild it again (after some changes for instance) run:

```bash
$ npm run build
```

To build bundle for production instead of for development run:

```bash
$ cd srv
$ npm install
$ npm run prod-build
$ npm run build-backend-templates
```

You also could run a watcher in background to rebuild client code automatically
when sources change by using this command (development mode):

```bash
$ npm run watch
```

To clean everything run:

```bash
$ npm run clean
$ npm run clean-backend-templates
```

### CI build

[CircleCI][ci] is configured to build CaRMa on every push. If you have
Docker installed, you may use [CircleCI CLI tool][ci-cli] to perform
builds in the same environment as on CI server:

    circleci build --job build_client
    circleci build --job build_server

Jobs run in an auto-removed container, so at the moment you may
only use them to test that the build finishes successfully.

### Docker bundle

*This is an EXPERIMENTAL workflow*

If you build the frontend and the backend Docker image, you can also
build a bundle image containing both:

    cd srv/
    docker build -t carma-bundle .

CaRMa server executable inside the container is located at
`/usr/local/bin/carma`.

This is what [`build_bundle`][ci-config] CI step does.

Alternatively, you may skip building CaRMa altogether and just pick an
image from [formalmethods/carma-bundle][hub-bundle] repo on Docker
Hub.

To run the bundle, use [carma-bundle.yml][] Docker Compose file, which
combines a CaRMa bundle and PostgreSQL.

Required steps:

1. Create carma-db-data volume using docker/dev-pg-9.3 container.

2. Clone [carma-configs][] and edit CaRMa configs path in `volumes:`
   section of [carma-bundle.yml][].

The command to run the bundle is

    docker-compose -f docker/carma-bundle.yml up

## Running

1. Get a recent database snapshot. Backup folder with daily production
   snapshots is mounted on `carma-test` server at
   `/var/backups/allbackups/postgresql_carma`:

   ```
   scp <YOUR_LOGIN>@192.168.10.13:/var/backups/allbackups/postgresql_carma/2017-05-29_03-15_carma.sql.gz .
   ```

[carma-bundle.yml]: docker/carma-bundle.yml
[carma-configs]: https://github.com/f-me/carma-configs
[ci-cli]: https://circleci.com/docs/2.0/local-jobs/#installing-the-cli-locally
[ci-config]: .circleci/config.yml
[ci]: https://circleci.com/gh/f-me/carma
[haskell-stack]: https://docs.haskellstack.org/en/stable/README/
[hub-bundle]: https://hub.docker.com/r/formalmethods/carma-bundle/tags/
