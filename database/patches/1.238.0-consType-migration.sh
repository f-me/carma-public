${PSQL} <<EOF

`cat baseline/3-dictionaries/66-ConsultationType.sql`

DROP VIEW "Услуги";

ALTER TABLE consultationtbl ADD COLUMN consType_tmp int4;

UPDATE consultationtbl SET consType_tmp=1 WHERE consType='operator';
UPDATE consultationtbl SET consType_tmp=2 WHERE consType='mech';

ALTER TABLE consultationtbl DROP COLUMN consType;

ALTER TABLE consultationtbl ADD COLUMN consType int4 REFERENCES "ConsultationType";
UPDATE consultationtbl SET consType = consType_tmp;
ALTER TABLE consultationtbl DROP COLUMN consType_tmp;

`cat baseline/5-views/7-ru-services.sql`

EOF
