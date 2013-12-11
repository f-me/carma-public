#!/bin/bash -e

$PSQL -f baseline/3-dictionaries/17-DefaultNewCaseField.sql

$PSQL <<EOF
  delete from "NewCaseField"
    where id not in
      (select min(id) from "NewCaseField" group by field, program);
  alter table "NewCaseField"
    add constraint NewCaseField_fp_unique UNIQUE (field, program);
  grant all on "NewCaseField" to carma_search;
  grant all on "NewCaseField_id_seq" to carma_search;
EOF
