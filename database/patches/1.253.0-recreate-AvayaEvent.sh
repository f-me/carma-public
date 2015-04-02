$PSQL <<EOF

BEGIN;

DROP TABLE "AvayaEvent";
`cat baseline/3-dictionaries/63-AvayaEvent.sql`

END;
EOF
