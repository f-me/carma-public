#
# /var/lib/postgresql/data supposed to be mounted
# and initialized before starting postgres daemon.
#
# How to initialize:
#   # docker build -t carma-db docker/dev-pg-9.3/
#   # docker volume create --name carma-db-data
#   # docker run --rm --entrypoint=/init -it -v carma-db-data:/var/lib/postgresql/data --name carma-db carma-db
#
# How to clean database data:
#   # docker run --rm --entrypoint=/clean -it -v carma-db-data:/var/lib/postgresql/data --name carma-db carma-db
#
# Clean and initialize again:
#   # docker run --rm --entrypoint=/reinit -it -v carma-db-data:/var/lib/postgresql/data --name carma-db carma-db
#
# How to just patch PostgreSQL config:
#   # docker run --rm -it --entrypoint=/patch-pg-config -v carma-db-data:/var/lib/postgresql/data --name carma-db carma-db
#
# How to run a container:
#   # docker run -d -p 127.0.0.1:5432:5432 -v carma-db-data:/var/lib/postgresql/data --name carma-db carma-db
#
# Or a temporary container:
#   # docker run --rm -it -p 127.0.0.1:5432:5432 -v carma-db-data:/var/lib/postgresql/data --name carma-db carma-db
# Upload a SQL dump:
#   # psql -h 127.0.0.1 -p 5432 -U carma_db_sync -d carma -f 2017-05-29_03-15_carma.sql

FROM postgres:9.3.17

RUN apt-get update \
    && apt-get install -y --no-install-recommends \
       postgresql-9.3-postgis-2.3 postgresql-9.3-postgis-2.3-scripts \
    && rm /docker-entrypoint.sh \
       /usr/local/bin/docker-entrypoint.sh /usr/local/bin/gosu \
    && rmdir /docker-entrypoint-initdb.d \
    && sed -i 's/^# *\(en_US\|ru_RU\)\(.UTF-8\)/\1\2/' /etc/locale.gen \
    && locale-gen

COPY patch-pg-config.sh init-db.sh start-pg.sh \
     patch-pg-config init clean reinit start /

EXPOSE 5432
USER postgres:postgres
ENTRYPOINT /start
