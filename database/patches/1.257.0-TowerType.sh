$PSQL <<EOF

BEGIN;

`cat baseline/3-dictionaries/71-TowerType.sql`

DROP VIEW "Услуги";
DROP VIEW allservicesview;

ALTER TABLE towagetbl ADD COLUMN towerType_tmp int4;

UPDATE towagetbl
SET towerType_tmp = 1
WHERE towerType = 'evac';

UPDATE towagetbl
SET towerType_tmp = 2
WHERE towerType = 'manip';

UPDATE towagetbl
SET towerType_tmp = 3
WHERE towerType = 'carts';

UPDATE towagetbl
SET towerType_tmp = 4
WHERE towerType = 'heavy';

UPDATE towagetbl
SET towerType_tmp = 5
WHERE towerType = 'longcat';

ALTER TABLE towagetbl DROP COLUMN towerType;
ALTER TABLE towagetbl ADD COLUMN towerType int4 REFERENCES "TowerType";
UPDATE towagetbl SET towerType = towerType_tmp WHERE towerType_tmp IS NOT NULL;
ALTER TABLE towagetbl DROP COLUMN towerType_tmp;

INSERT INTO "FieldPermission" (role, model, field, r, w)
SELECT role, 'TowerType', field, r, w
FROM "FieldPermission"
WHERE model = 'PaymentType';

END;
EOF
