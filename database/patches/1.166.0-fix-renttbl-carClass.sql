ALTER TABLE renttbl ADD COLUMN carClass_tmp int4;
UPDATE renttbl SET carClass_tmp = nullif(substring(regexp_replace(carClass, '\D', '', 'g') from 1 for 4),'')::int4;
UPDATE renttbl SET carClass_tmp = NULL WHERE coalesce(carClass, '') <> coalesce(carClass_tmp::text, '');
UPDATE renttbl SET carClass_tmp = NULL WHERE NOT EXISTS (SELECT 1 FROM "CarClass" WHERE id = carClass_tmp);
UPDATE renttbl SET carClass_tmp = p.id
FROM partnertbl p WHERE p.name = carClass;
ALTER TABLE renttbl ALTER COLUMN carClass TYPE int4 USING (carClass_tmp);
ALTER TABLE renttbl DROP COLUMN carClass_tmp;
ALTER TABLE renttbl
ADD CONSTRAINT "renttbl_carClass_fkey"
FOREIGN KEY (carClass) REFERENCES "CarClass" (id);
