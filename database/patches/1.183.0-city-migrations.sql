-- "City" schema fixes
ALTER TABLE "City"
ALTER COLUMN value SET DEFAULT '',
DROP CONSTRAINT "City_value_key",
ADD CONSTRAINT "City_label_nonempty" CHECK (label <> ''),
ADD COLUMN coords geometry(Point,4326);

UPDATE "City" SET timezone = '' WHERE timezone IS NULL;
ALTER TABLE "City"
ALTER COLUMN timezone SET DEFAULT '',
ALTER COLUMN timezone SET NOT NULL;

INSERT INTO "FieldPermission"
(role, model, field, r, w)
SELECT role, model, 'coords', r, w
FROM "FieldPermission"
WHERE model = 'City' AND field='timezone';

-- Convert usermetatbl.boCities to use numeric City ids
UPDATE usermetatbl
SET boCities = remap.newcities FROM
(SELECT array_agg(c.id)::text[] as newcities, bocities.userid
 FROM "City" c,
 (select unnest(boCities) as value, usermetatbl.id as userid from usermetatbl) bocities
 WHERE c.value = bocities.value
 GROUP BY bocities.userid) remap
WHERE id = remap.userid;

ALTER TABLE usermetatbl
ALTER COLUMN boCities TYPE int4[] USING(boCities::int4[]);

DROP VIEW "Партнеры";
DROP VIEW "Услуги с приоритетами";

UPDATE partnertbl SET city = NULL
WHERE NOT EXISTS (
SELECT 1 FROM "City"
WHERE lower(value) = lower(city) OR lower(label) = lower(city));
UPDATE partnertbl SET city = NULL WHERE city = '';
UPDATE partnertbl SET city = c.id::text FROM "City" c WHERE lower(c.value) = lower(city);
UPDATE partnertbl SET city = c.id::text FROM "City" c WHERE lower(c.label) = lower(city);
ALTER TABLE partnertbl
ALTER COLUMN city TYPE int4 USING(city::int4);

DROP VIEW "Звонки";

UPDATE calltbl SET city = NULL
WHERE NOT EXISTS (SELECT 1 FROM "City"
WHERE lower(value) = lower(city) OR lower(label) = lower(city));
UPDATE calltbl SET city = NULL WHERE city = '';
UPDATE calltbl SET city = c.id::text FROM "City" c WHERE lower(c.value) = lower(city);
UPDATE calltbl SET city = c.id::text FROM "City" c WHERE lower(c.label) = lower(city);
ALTER TABLE calltbl
ALTER COLUMN city TYPE int4 USING(city::int4);

DROP VIEW "Отказы партнеров";

UPDATE casetbl SET city = NULL
WHERE NOT EXISTS (SELECT 1 FROM "City"
WHERE lower(value) = lower(city) OR lower(label) = lower(city));
UPDATE casetbl SET city = NULL WHERE city = '';
UPDATE casetbl SET city = c.id::text FROM "City" c WHERE lower(c.value) = lower(city);
UPDATE casetbl SET city = c.id::text FROM "City" c WHERE lower(c.label) = lower(city);
ALTER TABLE casetbl
ALTER COLUMN city TYPE int4 USING(city::int4);
