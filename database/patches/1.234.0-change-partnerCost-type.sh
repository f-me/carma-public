$PSQL <<EOF

BEGIN;

DROP VIEW "Услуги";

ALTER TABLE servicetbl ALTER COLUMN payment_partnerCost TYPE double precision;

`cat baseline/5-views/7-ru-services.sql`

END;

EOF
