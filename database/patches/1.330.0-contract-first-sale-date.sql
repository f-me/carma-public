BEGIN;

ALTER TABLE "Contract" ADD COLUMN firstSaleDate date;
ALTER TABLE casetbl ADD COLUMN car_firstSaleDate date;

-- For Contract
INSERT INTO "FieldPermission" (role, model, field, r, w)
(SELECT role, model, 'firstSaleDate', r, w
 FROM "FieldPermission" WHERE model = 'Contract' AND field = 'model');

-- For Case
INSERT INTO "FieldPermission" (role, model, field, r, w)
(SELECT role, model, 'car_firstSaleDate', r, w
 FROM "FieldPermission" WHERE model = 'Case' AND field = 'car_buyDate');

/*
  See https://github.com/f-me/carma/issues/2812 (Formal Methods)

  Only for:
    - BMW New [590, 594]
    - BMW SARA [596, 555]
    - MINI New [600]
    - MINI SARA [602]

  carma=# select id,label,parent from "SubProgram" where parent = 171;
   id  |             label             | parent
  -----+-------------------------------+--------
   645 | Премиум Селекшн               |    171
   602 | МИНИ после ТО /MINI-SARA      |    171
   600 | МИНИ новые /MINI-NEW          |    171
   617 | МИНИ Вип /MINI VIP /n/a       |    171
   590 | БМВ новые /BMW - New          |    171
   610 | БМВ /МИНИ Вип  /BMW /MINI-VIP |    171
   615 | Ай серия /I Series            |    171
   596 | BMW Motorrad - мотоциклы SARA |    171
   555 | БМВ после ТО /BMW-SARA        |    171
   594 | BMW Motorrad - мотоциклы New  |    171
  (10 rows)
*/
INSERT INTO "SubProgramContractPermission"
(parent, contractfield, showtable, showform)
(SELECT parent, 'firstSaleDate', showtable, showform
 FROM "SubProgramContractPermission"
 WHERE contractfield = 'model'
       AND (parent = 590
         OR parent = 594
         OR parent = 596
         OR parent = 555
         OR parent = 600
         OR parent = 602
       )
);

-- Regenerating parent->children references.
UPDATE "SubProgram" SET contractPermissions = rev.perms FROM
(SELECT parent, array_agg(id) AS perms
 FROM "SubProgramContractPermission" GROUP BY parent) rev
 WHERE rev.parent = "SubProgram".id;

INSERT INTO "FieldPermission" (role, model, field, r, w)
(SELECT role, model, replace(field, 'model', 'firstSaleDate'), r, w
 FROM "FieldPermission" WHERE model='VinFormat' AND field ilike 'model%');

ALTER TABLE "VinFormat" ADD COLUMN firstSaleDateLoad bool NOT NULL DEFAULT FALSE;
ALTER TABLE "VinFormat" ADD COLUMN firstSaleDateTitle text NOT NULL DEFAULT 'Дата первой продажи';
ALTER TABLE "VinFormat" ADD COLUMN firstSaleDateDefault date;
ALTER TABLE "VinFormat" ADD COLUMN firstSaleDateRequired bool NOT NULL DEFAULT FALSE;

/*
  First-Sale-Date field loading enabled only for 'BMW' formats.

  carma=# select id, label from "VinFormat" where label ilike '%bmw%';
    id  |     label
  ------+---------------
     15 | bmw moto
   1011 | BMW 8 часов
   1059 | bmw mini new
   2022 | BMW MINI SARA
     25 | BMW VIP
  (5 rows)
*/
UPDATE "VinFormat" SET firstSaleDateLoad = TRUE
  WHERE id = 15
     OR id = 1011
     OR id = 1059
     OR id = 2022
     OR id = 25
        ;

-- Program #171 is BMW.
-- Ordering value '480' is between `car_seller` (474) and `car_plateNum` (490)
-- by filter `model = 1 AND program = 171` (model #1 is 'Case').
INSERT INTO "ConstructorFieldOption"
(model, program, ord, field, label, r, w)
(SELECT id, 171, 480, 'car_firstSaleDate', 'Дата первой продажи', TRUE, TRUE
 FROM "CtrModel" WHERE value = 'Case');

COMMIT;
