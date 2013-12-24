#!/bin/bash -e

$PSQL -c 'drop table  if exists "Sms"'
$PSQL -f baseline/3-dictionaries/27-Sms.sql
$PSQL << EOF
COPY "FieldPermission" (role, model, field, r, w) FROM stdin;
1	Sms	id	t	f
1	Sms	caseRef	t	t
1	Sms	phone	t	t
1	Sms	template	t	t
1	Sms	msgText	t	t
1	Sms	status	t	t
\.
EOF
