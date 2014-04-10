#!/bin/bash -e

# This script must be run from recepient host!

FROMHOST=192.168.10.15
FROMDB=carma

TOHOST=localhost
TODB=carma

# VINNIE:
DUMP=$(cat <<EOF
create table "Contract_tmp" as select * from "Contract" where ctime > '2014-02-24' and committer = 193;
create table "VinFormat_tmp" as select * from "VinFormat" where id <> 1000;
create table "CarMake_syn" as select id, synonyms from "CarMake" where synonyms is not null;
create table "CarModel_syn" as select id, synonyms from "CarModel" where synonyms is not null;
create table "Engine_syn" as select id, synonyms from "Engine" where synonyms is not null;
create table "Transmission_syn" as select id, synonyms from "Transmission" where synonyms is not null;
create table "SubProgram_syn" as select id, synonyms from "SubProgram" where synonyms is not null;
create table "partnertbl_syn" as select id, name, synonyms from "partnertbl";
EOF
)

psql -h ${FROMHOST} -U carma_db_sync ${FROMDB} -c "${DUMP}"
pg_dump -h ${FROMHOST} -U carma_db_sync ${FROMDB} -t '"Contract_tmp"' -t '"VinFormat_tmp"' -t '"CarMake_syn"' -t '"CarModel_syn"' -t '"Engine_syn"' -t '"Transmission_syn"' -t '"SubProgram_syn"' -t '"partnertbl_syn"' > /tmp/Transfer.sql
psql -h ${FROMHOST} -U carma_db_sync ${FROMDB} -c 'DROP TABLE "Contract_tmp","VinFormat_tmp","CarMake_syn","CarModel_syn","Engine_syn","Transmission_syn","SubProgram_syn","partnertbl_syn";'


# TARGET:
DUMP=$(cat <<EOF
insert into "Contract" select * from "Contract_tmp";
insert into "VinFormat" select * from "VinFormat_tmp";
update "CarMake" set synonyms = "CarMake_syn".synonyms from "CarMake_syn" where "CarMake".id="CarMake_syn".id;
update "CarModel" set synonyms = "CarModel_syn".synonyms from "CarModel_syn" where "CarModel".id="CarModel_syn".id;
update "Engine" set synonyms = "Engine_syn".synonyms from "Engine_syn" where "Engine".id="Engine_syn".id;
update "Transmission" set synonyms = "Transmission_syn".synonyms from "Transmission_syn" where "Transmission".id="Transmission_syn".id;
update "SubProgram" set synonyms = "SubProgram_syn".synonyms from "SubProgram_syn" where "SubProgram".id="SubProgram_syn".id;
update "partnertbl" set synonyms = "partnertbl_syn".synonyms, name = "partnertbl_syn".name  from "partnertbl_syn" where "partnertbl".id="partnertbl_syn".id;
DROP TABLE "Contract_tmp","VinFormat_tmp","CarMake_syn","CarModel_syn","Engine_syn","Transmission_syn","SubProgram_syn","partnertbl_syn";
EOF
)

psql -h ${TOHOST} -U carma_db_sync ${TODB} -f /tmp/Transfer.sql
echo "Writing to target db NOW"
psql -h ${TOHOST} -U carma_db_sync ${TODB} -c "${DUMP}"
