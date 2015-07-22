CREATE TABLE "BusinessRole"
(
    id       SERIAL PRIMARY KEY
  , label    text NOT NULL
);

INSERT INTO "BusinessRole" (id, label) VALUES
  (1, 'Front Office')
, (2, 'Back Office: Заказ услуг')
, (3, 'Back Office: Заказ вторичных услуг')
, (4, 'Back Office: Контроль услуг')
;

GRANT ALL ON "BusinessRole" TO carma_db_sync;
GRANT ALL ON "BusinessRole_id_seq" TO carma_db_sync;
