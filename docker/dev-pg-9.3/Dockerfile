# Read README.md before use!
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
