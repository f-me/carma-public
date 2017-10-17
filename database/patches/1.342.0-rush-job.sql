BEGIN;


-- Add 'rushJob' field for services
ALTER TABLE servicetbl ADD COLUMN rushJob BOOL DEFAULT FALSE;


-- value = 'core' (Экран кейса и базовые поля)
INSERT INTO "FieldPermission"
  (role, model, field, r, w)
  ( SELECT
      role.id, 'Service', 'rushJob', TRUE, FALSE
      FROM "Role" AS role WHERE value = 'core'
      LIMIT 1
  );


-- Shifting old order from place where new 'rushJob' will appear
WITH t AS (
  SELECT DISTINCT ON (t.id) t.model   AS model
                          , t.id      AS id
                          , t.field   AS field
                          , t.program AS program
                          , t.ord     AS ord
    FROM "ConstructorFieldOption" AS t
    INNER JOIN "CtrModel" AS ctr
          ON ctr.value IN ( 'Tech', 'Towage', 'Rent', 'Taxi'
                          , 'SoberDriver', 'AverageCommissioner'
                          )
    WHERE t.field = 'contractor_partner'
    ORDER BY
      t.id      ASC,
      t.program ASC,
      t.ord     ASC
  )

  UPDATE "ConstructorFieldOption" AS c
    SET ord = c.ord + 1
    FROM t
    WHERE c.model   = t.model
      AND c.program = t.program
      AND c.ord     > t.ord
  ;


-- Add 'rushJob' field after 'contractor_partner'
WITH t AS (
  SELECT DISTINCT ON (t.id) t.model   AS model
                          , t.id      AS id
                          , t.field   AS field
                          , t.program AS program
                          , t.ord     AS ord
    FROM "ConstructorFieldOption" AS t
    INNER JOIN "CtrModel" AS ctr
          ON ctr.value IN ( 'Tech', 'Towage', 'Rent', 'Taxi'
                          , 'SoberDriver', 'AverageCommissioner'
                          )
    WHERE t.field = 'contractor_partner'
    ORDER BY
      t.id      ASC,
      t.program ASC,
      t.ord     ASC
  )

  INSERT INTO "ConstructorFieldOption"
    (model, program, ord, field, label, r, w)
    ( SELECT t.model
           , t.program
           , t.ord + 1
           , 'rushJob'
           , 'Аврал'
           , TRUE
           , FALSE
        FROM t
    )
  ;


-- Add field where 'rushJob' flags mapped to specific cities at the moment
ALTER TABLE "ProcessingConfig"
  ADD COLUMN rushJobCities
  INTEGER[] NOT NULL DEFAULT '{}'::INTEGER[];


-- call = Оператор Front Office
-- head = Глава РКЦ
-- back = Работа с бэкофисом
INSERT INTO "FieldPermission"
  (role, model, field, r, w)
  ( SELECT role.id
         , 'ProcessingConfig'
         , 'rushJobCities'
         , TRUE
         , (CASE role.value WHEN 'head' THEN TRUE ELSE FALSE END)
      FROM "Role" AS role WHERE value IN ('call', 'back', 'head')
  );


COMMIT;
-- bump
