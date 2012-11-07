#!/bin/sh

psql -h localhost -d carma -U carma_db_sync -f alter-psql-satisfied
