#!/bin/sh

psql -d carma -U $1 < cleanup_partner_services_parentid.sql
