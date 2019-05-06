# dev-pg-9.3

Dockerized fixed PostgreSQL version which CaRMa uses.

`/var/lib/postgresql/data` directory of this container is supposed to be binded
to some directory on host machine and [initialized](#how-to-deploy)
before [starting `postgres` daemon](#running-container)
(to make database being persistent).

## <a name="how-to-deploy"></a>How to deploy this container

All steps to build a container, creating a virtual volume to store database data
persistently and initializing database in that volume.

Building container:
```bash
docker build -t carma-db docker/dev-pg-9.3/
```

Creating virtual volume (to store database data persistently):
```bash
docker volume create --name carma-db-data
```

<a name="initialize-database"></a>Initializing database in that volume:
```bash
docker run --rm --entrypoint=/init -it -v carma-db-data:/var/lib/postgresql/data --name carma-db carma-db
```

See [how to deploy database dump](#deploying-database-dump).

## Manipulation of database data and config

### How to clean database data

To scrap your data
(you have to [initialize](#initialize-database) database then).

```bash
docker run --rm --entrypoint=/clean -it -v carma-db-data:/var/lib/postgresql/data --name carma-db carma-db
```

### How to clean and initialize again

To start from scratch.

```bash
docker run --rm --entrypoint=/reinit -it -v carma-db-data:/var/lib/postgresql/data --name carma-db carma-db
```

### How to just patch PostgreSQL config

```bash
docker run --rm -it --entrypoint=/patch-pg-config -v carma-db-data:/var/lib/postgresql/data --name carma-db carma-db
```

## <a name="running-container"></a>Running a container

### How to run a container

```bash
docker run -d -p 127.0.0.1:5432:5432 -v carma-db-data:/var/lib/postgresql/data --name carma-db carma-db
```

#### Or a temporary container

By using `--rm` flag and `-it` instead of `-d` to make it being not daemonized
but being interactive (`Ctrl-C` interruption will remove the container but not
mounted database, so data will continue live, you will be able to start new
container at any time with the same data).

```bash
docker run --rm -it -p 127.0.0.1:5432:5432 -v carma-db-data:/var/lib/postgresql/data --name carma-db carma-db
```

## <a name="deploying-database-dump"></a>Deploying database dump

Before starting to work with database you're supposed to deploy fresh database
snapshot first.

### How to upload SQL dump

```bash
psql -h 127.0.0.1 -p 5432 -U carma_db_sync -d carma -f 2017-05-29_03-15_carma.sql
```

#### In case a database dump is gzipped

```bash
zcat 2017-05-29_03-15_carma.sql.gz | psql -h 127.0.0.1 -p 5432 -U carma_db_sync -d carma
```
