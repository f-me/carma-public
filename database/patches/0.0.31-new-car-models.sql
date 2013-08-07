
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'grandCMax',
         'GRAND C-Max',
         id
    FROM "CarMaker" WHERE value = 'ford';

INSERT INTO "CarModel" (value, label, parent)
  SELECT 'f150',
         'F150',
         id
    FROM "CarMaker" WHERE value = 'ford';

INSERT INTO "CarModel" (value, label, parent)
  SELECT 'С-Elysee',
         'С-Elysee',
         id
    FROM "CarMaker" WHERE value = 'citroen';
