CREATE TABLE "AbuseTarget"
  (id SERIAL PRIMARY KEY
  ,label text UNIQUE NOT NULL
  );

INSERT INTO "AbuseTarget" (label) VALUES
  ('Дилер'), ('РАМК'), ('Партнёр'), ('Другое');

