#!/bin/bash -e

${PSQL} -f baseline/3-dictionaries/41-SubProgramContractPermission.sql
${PSQL} -c 'DROP TABLE "FieldPermission"'
${PSQL} -f baseline/3-dictionaries/8-FieldPermission.sql
