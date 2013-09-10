#!/bin/bash -e

$PSQL -c 'drop table if exists "NewCaseField"'

$PSQL -f baseline/3-dictionaries/15-NewCaseField.sql

$PSQL << EOF
insert into "NewCaseField" (program, field, label, r, w, required)
  select p.id, f.field, f.label, f.r, f.w, f.required
    from "NewCaseField" f, programtbl p
    where p.id > 1;

GRANT ALL ON "NewCaseField" TO carma_db_sync;
GRANT ALL ON "NewCaseField" TO carma_search;
EOF
