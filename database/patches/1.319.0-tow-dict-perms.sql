BEGIN;

INSERT INTO "FieldPermission" (role, model, field, r, w)
(SELECT role, 'BikeTowType', field, r, w FROM "FieldPermission" WHERE model='TowType');

INSERT INTO "FieldPermission" (role, model, field, r, w)
(SELECT role, 'TowSort', field, r, w FROM "FieldPermission" WHERE model='TowType');

END;
