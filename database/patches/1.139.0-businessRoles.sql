BEGIN;
ALTER TABLE usermetatbl DROP COLUMN businessRole;
ALTER TABLE usermetatbl
  ADD  COLUMN businessRole integer REFERENCES "BusinessRole";

ALTER TABLE "BusinessRole" ADD COLUMN roles integer[];

DELETE FROM "BusinessRole";
INSERT INTO "BusinessRole" (label, roles)
  VALUES ('Front Office',                       '{1,2,40}'),
         ('Back Office: Заказ услуг',           '{1,14,16,23,40}'),
         ('Back Office: Заказ вторичных услуг', '{1, 14, 16, 40}'),
         ('Back Office: Контроль услуг',        '{1, 14, 16, 40}');

-- Add permission on roles field to supervisor
INSERT INTO "FieldPermission" (role, model, field, r, w)
  VALUES (12, 'BusinessRole', 'roles', '1', '1');
END;
