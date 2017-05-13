BEGIN;

ALTER TABLE "BikeTowage"
ADD COLUMN canPushBike bool NOT NULL DEFAULT false,
ADD COLUMN canUnblock bool NOT NULL DEFAULT false,
ADD COLUMN canHelp bool NOT NULL DEFAULT false,
ADD COLUMN compl4 bool NOT NULL DEFAULT false,
ADD COLUMN compl5 bool NOT NULL DEFAULT false;

INSERT INTO "FieldPermission" (role, model, field, r, w)
(SELECT role, 'BikeTowage', 'canPushBike', r, w
 FROM "FieldPermission" WHERE model='BikeTowage' AND field = 'isCountryRide');

INSERT INTO "FieldPermission" (role, model, field, r, w)
(SELECT role, 'BikeTowage', 'canUnblock', r, w
 FROM "FieldPermission" WHERE model='BikeTowage' AND field = 'isCountryRide');

INSERT INTO "FieldPermission" (role, model, field, r, w)
(SELECT role, 'BikeTowage', 'canHelp', r, w
 FROM "FieldPermission" WHERE model='BikeTowage' AND field = 'isCountryRide');

INSERT INTO "FieldPermission" (role, model, field, r, w)
(SELECT role, 'BikeTowage', 'compl4', r, w
 FROM "FieldPermission" WHERE model='BikeTowage' AND field = 'isCountryRide');

INSERT INTO "FieldPermission" (role, model, field, r, w)
(SELECT role, 'BikeTowage', 'compl5', r, w
 FROM "FieldPermission" WHERE model='BikeTowage' AND field = 'isCountryRide');

INSERT INTO "ConstructorFieldOption" (model, program, ord, field, label, r, w)
(SELECT 20, program, ord + 1, 'canPushBike', 'Возможно ли катить мотоцикл', r, w
 FROM "ConstructorFieldOption" WHERE model=20 AND field='isCountryRide');

INSERT INTO "ConstructorFieldOption" (model, program, ord, field, label, r, w)
(SELECT 20, program, ord + 2, 'canUnblock', 'Возможно ли разблокировать руль', r, w
 FROM "ConstructorFieldOption" WHERE model=20 AND field='isCountryRide');

INSERT INTO "ConstructorFieldOption" (model, program, ord, field, label, r, w)
(SELECT 20, program, ord + 3, 'canHelp', 'Сможете ли помочь при погрузке и креплении', r, w
 FROM "ConstructorFieldOption" WHERE model=20 AND field='isCountryRide');

INSERT INTO "ConstructorFieldOption" (model, program, ord, field, label, r, w)
(SELECT 20, program, ord + 4, 'compl4', 'Сложность №4', 'f', 'f'
 FROM "ConstructorFieldOption" WHERE model=20 AND field='isCountryRide');

INSERT INTO "ConstructorFieldOption" (model, program, ord, field, label, r, w)
(SELECT 20, program, ord + 5, 'compl5', 'Сложность №5', 'f', 'f'
 FROM "ConstructorFieldOption" WHERE model=20 AND field='isCountryRide');

UPDATE "ConstructorFieldOption" o SET ord = j.ord + 1
FROM (SELECT ord, program FROM "ConstructorFieldOption" WHERE model = 20 AND field = 'compl5') j
WHERE o.program = j.program AND o.model = 20 AND o.field = 'bikeTowType';

-- Fix weird leftovers - for some programs there're no constructor
-- entries for bikeTowType field
INSERT INTO "ConstructorFieldOption" (model, program, ord, field, label, r, w)
(SELECT 20, program, ord + 1, 'bikeTowType', 'Тип мотоэвакуации', r, w
 FROM "ConstructorFieldOption" s where model=20 AND field='compl5'
 AND NOT EXISTS (SELECT 1 FROM "ConstructorFieldOption" q WHERE q.program = s.program AND field='bikeTowType'));

COMMIT;
