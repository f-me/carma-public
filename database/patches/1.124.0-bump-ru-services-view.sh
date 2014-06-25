#!/bin/bash

${PSQL} -c 'DROP VIEW IF EXISTS "Услуги"'
${PSQL} -f baseline/5-views/7-ru-services.sql
