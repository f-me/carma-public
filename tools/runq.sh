#!/bin/sh

SQL=$1
shift
ARGS=$@

psql -d carma $@ < $SQL
