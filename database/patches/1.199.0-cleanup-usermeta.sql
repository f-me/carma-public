ALTER TABLE usermetatbl ADD COLUMN subPrograms int4[] NOT NULL DEFAULT '{}'::integer[];
UPDATE usermetatbl SET subprograms = programs;
ALTER TABLE usermetatbl DROP COLUMN programs;

UPDATE "FieldPermission" SET field='subPrograms' WHERE model='Usermeta' and field='programs';

ALTER TABLE usermetatbl DROP COLUMN lastactivity;
ALTER TABLE usermetatbl DROP COLUMN lastlogout;
ALTER TABLE usermetatbl DROP COLUMN weathercities;
