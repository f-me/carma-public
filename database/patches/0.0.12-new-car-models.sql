
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'bMax',
         'B-Max',
         id
    FROM "CarMaker" WHERE value = 'ford';

INSERT INTO "CarModel" (value, label, parent)
  SELECT 'f250',
         'F250',
         id
    FROM "CarMaker" WHERE value = 'ford';

UPDATE "CarModel" SET label = 'GT' where value = 'fordGT';
