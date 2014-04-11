INSERT INTO "CarMake" (value, label) VALUES ('Acura', 'Acura');
INSERT INTO "CarMake" (value, label) VALUES ('Gaz', 'ГАЗ');
INSERT INTO "CarMake" (value, label) VALUES ('Zaz', 'ЗАЗ');
INSERT INTO "CarMake" (value, label) VALUES ('Msk', 'Москвич');


INSERT INTO "CarModel" (value, label, parent)
(SELECT 'Element', 'Element', id FROM "CarMake" WHERE value='honda');


INSERT INTO "CarModel" (value, label, parent)
(SELECT 'Venga', 'Venga', id FROM "CarMake" WHERE value='kia');


INSERT INTO "CarModel" (value, label, parent)
(SELECT 'Liberty', 'Liberty', id FROM "CarMake" WHERE value='nissan');

INSERT INTO "CarModel" (value, label, parent)
(SELECT 'March', 'March', id FROM "CarMake" WHERE value='nissan');


INSERT INTO "CarModel" (value, label, parent)
(SELECT 'Felicia', 'Felicia', id FROM "CarMake" WHERE value='skoda');


INSERT INTO "CarModel" (value, label, parent)
(SELECT 'Gaia', 'Gaia', id FROM "CarMake" WHERE value='toyota');


INSERT INTO "CarModel" (value, label, parent)
(SELECT 'S12', 'S12', id FROM "CarMake" WHERE value='Chery');


INSERT INTO "CarModel" (value, label, parent)
(SELECT 'MDX', 'MDX', id FROM "CarMake" WHERE value='Acura');


INSERT INTO "CarModel" (value, label, parent)
(SELECT '31029', '31029', id FROM "CarMake" WHERE value='Gaz');


INSERT INTO "CarModel" (value, label, parent)
(SELECT 'Chance', 'Шанс', id FROM "CarMake" WHERE value='Zaz');


INSERT INTO "CarModel" (value, label, parent)
(SELECT '2141', '2141', id FROM "CarMake" WHERE value='Msk');
