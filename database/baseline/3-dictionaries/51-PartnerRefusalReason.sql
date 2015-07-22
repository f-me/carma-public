CREATE TABLE "PartnerRefusalReason"
  ( id       SERIAL PRIMARY KEY
  , label    text UNIQUE NOT NULL CHECK (label <> '')
  );

GRANT ALL ON "PartnerRefusalReason" TO carma_db_sync;
GRANT ALL ON "PartnerRefusalReason_id_seq" TO carma_db_sync;

INSERT INTO "PartnerRefusalReason" (label) VALUES
    ('Занято (Постоянно короткие гудки)')
  , ('Нет свободных эвакуаторов')
  , ('Время подачи эвакуатора больше часа')
  , ('Подрядчик не предоставляет данную услугу')
  , ('Не работают в данное время')
  , ('Эвакуатор не может взять такой автомобиль (VW Crafter, Peugeot Boxer, Ford Transit)')
  , ('Эвакуатор сломан')
  , ('Телефон не отвечает')
  , ('Нет свободных машин техпомощи')
  , ('Другое')
;
