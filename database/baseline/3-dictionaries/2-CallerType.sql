CREATE TABLE "CallerType"
  (id    SERIAL PRIMARY KEY
  ,label text UNIQUE NOT NULL
  );

INSERT INTO "CallerType" (id, label) VALUES
  (1, 'Клиент'),
  (2, 'Партнёр'),
  (3, 'Дилер'),
  (4, 'Сотрудник'),
  (5, 'Другое');

GRANT ALL ON "CallerType" TO carma_db_sync;
