BEGIN;


ALTER TABLE servicetbl ADD COLUMN rushJob BOOL DEFAULT FALSE;


-- id#1 ("Role") - core/Экран кейса и базовые поля
INSERT INTO "FieldPermission"
  (role, model, field, r, w)
  VALUES
  (1, 'Service', 'rushJob', TRUE, FALSE);


-- Shifting old order from place where new 'rushjob' will appear
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


COMMIT;
