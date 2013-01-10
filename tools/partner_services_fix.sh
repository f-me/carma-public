#!/bin/sh

psql -d carma -U $1 < partner_services_fix.sql
