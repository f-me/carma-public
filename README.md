# CaRMa

[![Circle CI](https://circleci.com/gh/ruamk/carma.svg?style=shield&circle-token=ed097e1dbbde9591b7b2bec9ce252ddc840deb54)][ci]
[![](https://images.microbadger.com/badges/image/formalmethods/carma-bundle.svg)](https://microbadger.com/images/formalmethods/carma-bundle)

## Building

Refer to [`.circleci/config.yml`][ci-config] for full building instructions.

### Backend (Haskell)

#### Generic

##### Prepare database

1. Install PostgreSQL 9.3 (this is what production servers use) and
   PostGIS (extension for PostgreSQL).

   We have [docker container][docker/dev-pg-9.3] with exact PostgreSQL version
   for development, you could use it.

   Create a database named `carma`:

   ```postgresql
   createdb carma
   ```

2. Add roles:

   ```postgresql
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

3. Set passwords:

   ```bash
   psql carma -c "alter user carma with password 'pass'"
   psql carma -c "alter user carma_db_sync with password 'pass'"
   ```

4. Unpack and recover a database snapshot (note that this needs to run
   as a PostgreSQL superuser - for example, prepend with `sudo -u postgres`):

   ```bash
   psql carma -f 2017-05-29_03-15_carma.sql
   ```

   Or if you use [docker/dev-pg-9.3][] mentioned above,
   you could do it by this command:

   ```bash
   psql -h 127.0.0.1 -U carma_db_sync -d carma -f 2017-05-29_03-15_carma.sql
   ```

   In case you have a gzipped database dump (`.sql.gz` extension):

   ```bash
   zcat 2017-05-29_03-15_carma.sql.gz | psql carma
   ```

   For [docker/dev-pg-9.3][]:

   ```bash
   zcat 2017-05-29_03-15_carma.sql.gz | psql -h 127.0.0.1 -U carma_db_sync -d carma
   ```

5. You might need to apply some migrations if your database snapshot is older
   than current state of git-branch. To do so run:

   ```bash
   (cd database && ./db.sh update)
   ```

   Or if you use [docker/dev-pg-9.3][]:

   ```bash
   (cd database && env PGHOST=127.0.0.1 PGUSER=carma_db_sync PGPASSWORD=pass ./db.sh update-devel)
   ```

##### Build backend executables

1. Install [Haskell Stack][haskell-stack]. Check that it works:

   ```bash
   stack --numeric-version
   # 1.6.3
   ```

2. Clone **CaRMa** Git repository:

   ```bash
   git clone git@github.com:ruamk/carma
   cd carma
   ```

3. Add log files:

   ```bash
   mkdir -p srv/log && > srv/log/access.log && > srv/log/error.log
   ```

4. Build the backend:

   ```bash
   stack build --install-ghc
   ```

   P.S. When you rebuild backend on production servers you need to run

   ```bash
   stack install
   ```

   after to install executables to `$HOME/.local/bin` dir which is used to run
   backend from by init-scripts.

5. Copy default configuration files to your local directory
   (which is ignored from git-index):

   ```bash
   cp -r srv/snaplets-default srv/snaplets
   ```

   Keep in mind that if you change database configuration or credentials to
   access to it you need to update these files (links points to default ones):

   - [srv/snaplets/persist/devel.cfg](./srv/snaplets-default/persist/devel.cfg)
   - [srv/snaplets/postgresql-simple/devel.cfg](./srv/snaplets-default/postgresql-simple/devel.cfg)

6. **WARNING!** Before going to next step you supposed to build
   [frontend](#frontend) first, because templates for backend must be built
   by frontend toolchain before start.

7. Run the server from `srv/` directory:

   ```bash
   (cd srv && stack exec carma -- -p 8000)
   ```

8. Check that the server works:

   ```bash
   curl localhost:8000/meta
   ```

#### macOS

On macOS with `openssl` and `icu4c` installed via Homebrew, build with

```bash
stack build --extra-include-dirs=/usr/local/opt/openssl/include/ --extra-include-dirs=/usr/local/opt/icu4c/include/ --extra-lib-dirs=/usr/local/opt/openssl/lib/ --extra-lib-dirs=/usr/local/opt/icu4c/lib/
```

#### Docker

Alternatively, to build the server inside Docker and package all
executables in a container:

```bash
stack docker pull
stack --docker image container
```

If you're on Linux, you can just build with `stack --docker build` and
run the app outside Docker. You will not be able to run the app built
this way on macOS or Windows.

If the container has no access to your SSH keys (which may be the case
when using a token), use `stack dot` prior to building the image to
quickly fetch dependencies (`.stack-work/downloaded` directory is
shared between the container and the host anyways).

Note that this container lacks frontend resources necessary to launch
**CaRMa** - see below for how to build the whole bundle.

### Frontend

Check that you have proper versions of **node.js** and **npm**:

```bash
printf 'node: %s, npm: %s\n' "`node --version`" "`npm --version`"
# node: v8.9.1, npm: 5.6.0
```

To build front-end from scratch (development build):

```bash
cd srv
npm install
npm run build
npm run build-backend-templates
```

If you need to just rebuild it again (after some changes for instance) run:

```bash
npm run build
```

To build bundle for production instead of for development run:

```bash
cd srv
npm install
npm run prod-build
npm run build-backend-templates
```

If you're about to deploy next release to the production server, just run
(this includes `npm i`, `clean-build-backend-templates` and `prod-clean-build`):

```bash
npm run prod-release
```

You also could run a watcher in background to rebuild client code automatically
when sources change by using this command (development mode):

```bash
npm run watch
```

To clean everything run:

```bash
npm run clean
npm run clean-backend-templates
```

There's also single command to do the same:

```bash
npm run full-clean
```

#### "Pure" frontend

**WARNING!** Do not forget to also build "pure" frontend,
the commands look almost the same as you saw above.
See ["pure" frontend README][pure-readme] for details.

This is partly rewritten frontend (some rewritten parts of it),
some parts of it included to the main frontend using `<iframe>`s.

Ideally the whole frontend must be rewritten in this "pure" version.

### CI build

[CircleCI][ci] is configured to build CaRMa on every push. If you have
Docker installed, you may use [CircleCI CLI tool][ci-cli] to perform
builds in the same environment as on CI server:

```bash
circleci build --job build_client
circleci build --job build_server
```

Jobs run in an auto-removed container, so at the moment you may
only use them to test that the build finishes successfully.

### Docker bundle

*This is an EXPERIMENTAL workflow*

If you build the frontend and the backend Docker image, you can also
build a bundle image containing both:

```bash
cd srv/
docker build -t carma-bundle .
```

CaRMa server executable inside the container is located at
`/usr/local/bin/carma`.

This is what [`build_bundle`][ci-config] CI step does.

Alternatively, you may skip building CaRMa altogether and just pick an
image from [formalmethods/carma-bundle][hub-bundle] repo on Docker
Hub.

To run the bundle, use [carma-bundle.yml][] Docker Compose file, which
combines a CaRMa bundle and PostgreSQL.

Required steps:

1. Create carma-db-data volume using [docker/dev-pg-9.3][] container.

2. Clone [carma-configs][] and edit CaRMa configs path in `volumes:`
   section of [carma-bundle.yml][].

The command to run the bundle is

```bash
docker-compose -f docker/carma-bundle.yml up
```

## Running

1. Get a recent database snapshot. Backup folder with daily production
   snapshots is mounted on `carma-test` server at
   `/var/backups/allbackups/postgresql_carma`:

   ```bash
   scp <YOUR_LOGIN>@192.168.10.13:/var/backups/allbackups/postgresql_carma/2017-05-29_03-15_carma.sql.gz .
   ```

[carma-bundle.yml]: docker/carma-bundle.yml
[carma-configs]: https://github.com/ruamk/carma-configs
[ci-cli]: https://circleci.com/docs/2.0/local-jobs/#installing-the-cli-locally
[ci-config]: .circleci/config.yml
[ci]: https://circleci.com/gh/ruamk/carma
[haskell-stack]: https://docs.haskellstack.org/en/stable/README/
[hub-bundle]: https://hub.docker.com/r/formalmethods/carma-bundle/tags/
[docker/dev-pg-9.3]: docker/dev-pg-9.3
[pure-readme]: srv/resources/assets/pure/README.md
