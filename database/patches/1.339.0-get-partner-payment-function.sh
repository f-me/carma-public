#!/usr/bin/env bash

$PSQL -c 'DROP VIEW IF EXISTS "PartnerPayment"' # drop old view
$PSQL -f baseline/2-functions/3-get-partner-payment.sql
