DELETE FROM "ConstructorFieldOption" WHERE screen = 1;

ALTER TABLE "ConstructorFieldOption" DROP COLUMN screen;

drop table "CtrScreen";
UPDATE "Dictionary" set majorFields=ARRAY['id', 'model', 'program', 'label']
WHERE id=4;
