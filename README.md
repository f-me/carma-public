# CaRMa

[![Circle CI](https://circleci.com/gh/f-me/carma.svg?style=svg&circle-token=ed097e1dbbde9591b7b2bec9ce252ddc840deb54)](https://circleci.com/gh/f-me/carma)

## Building

Refer to `circle.yml` for full building instructions.

### Backend (Haskell)

#### Generic

1. Install [Haskell Stack][haskell-stack]. Check that it works:

   ```
   stack --numeric-version
   1.4.0
   ```

2. Install PostgreSQL 9.3 (this is what production servers use) and PostGIS. Create a database named `carma`:

   ```
   createdb carma
   ```

3. Add roles:

   ```
   createuser carma
   createuser carma_db_sync
   createuser carma_geo
   createuser carma_sms
   createuser mail_svc
   createuser fmuser
   createuser pavel.golovnin
   createuser reportgen
   ```

3. Add passwords:

   ```
   psql carma -c "alter user carma with password 'pass'"
   psql carma -c "alter user carma_db_sync with password 'pass'"
   ```

3. Unpack and recover a database snapshot:

   ```
   psql carma -f 2017-05-29_03-15_carma.sql
   ```

3. Clone CaRMa Git repository:

   ```
   git clone git@github.com:f-me/carma.git
   cd carma
   ```

3. Add log files

   ```
   mkdir srv/log && > srv/log/access.log && > srv/log/error.log
   ```

4. Build the backend:

   ```
   stack setup
   stack install
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
install && cd -` (just once), then `npm
install && bower update && grunt build` to rebuild all client
code. Run `grunt watch` in background to rebuild client code
automatically when sources change.

## Running

1. Get a recent database snapshot. Backup folder with daily production
   snapshots is mounted on `carma-test` server at
   `/var/backups/allbackups/postgresql_carma`:

   ```
   scp <YOUR_LOGIN>@192.168.10.13:/var/backups/allbackups/postgresql_carma/2017-05-29_03-15_carma.sql.gz .
   ```

[haskell-stack]: https://docs.haskellstack.org/en/stable/README/
