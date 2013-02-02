#!/bin/sh

psql -h localhost -d carma -U carma_db_sync -f tech1consultation.sql
