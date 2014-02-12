with inherited as (
SELECT column_parent.attname  AS col
FROM pg_inherits
JOIN pg_class parent            ON pg_inherits.inhparent  = parent.oid
JOIN pg_class child             ON pg_inherits.inhrelid   = child.oid
JOIN pg_namespace nmsp_parent   ON nmsp_parent.oid        = parent.relnamespace
JOIN pg_namespace nmsp_child    ON nmsp_child.oid         = child.relnamespace
JOIN pg_attribute column_parent ON column_parent.attrelid = parent.oid
WHERE column_parent.attnum > 0 and child.relname = 'towagetbl'
AND column_parent.attname NOT ILIKE '%pg.dropped%'
),
own as (
SELECT column_name AS col FROM information_schema.columns
WHERE table_name ='towagetbl'
and column_name not in (select col from inherited)
),
upd as (
INSERT INTO "FieldPermission" (role, model, field, r, w)
       (SELECT role, 'Service' as model, field, r, w
       FROM "FieldPermission"
       WHERE model = 'towage'
       AND   lower(field) in (select col from inherited))
)
INSERT INTO "FieldPermission" (role, model, field, r, w)
       (SELECT role, 'Towage' as model, field, r, w
       FROM "FieldPermission"
       WHERE model = 'towage'
       AND   lower(field) in (select col from own));
