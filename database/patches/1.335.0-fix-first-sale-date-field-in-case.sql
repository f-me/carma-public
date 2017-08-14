BEGIN;

-- See also 1.330.0-contract-first-sale-date.sql

INSERT INTO "ConstructorFieldOption"
(model, program, ord, field, label, r, w)
(SELECT ctrl.id,
        program.id,
        480,
        'car_firstSaleDate',
        'Дата первой продажи',
        FALSE,
        FALSE
 FROM "CtrModel" AS ctrl
 INNER JOIN "Program" AS program
       ON NOT EXISTS (SELECT ex.id
                       FROM "ConstructorFieldOption" AS ex
                       WHERE ex.field   = 'car_firstSaleDate'
                         AND ex.program = program.id
                       LIMIT 1
                      )
 WHERE ctrl.value = 'Case'
);

COMMIT;
