CREATE TABLE "AbuseTarget"
  (id SERIAL PRIMARY KEY
  ,label text UNIQUE NOT NULL
  );

INSERT INTO "AbuseTarget" (label) VALUES
  ('Дилер'), ('РАМК'), ('Партнёр'), ('Другое');


GRANT ALL ON "AbuseTarget" TO carma_db_sync;
