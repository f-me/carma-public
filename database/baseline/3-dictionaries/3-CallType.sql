CREATE TABLE "CallType"
  (id     SERIAL PRIMARY KEY
  ,label  text NOT NULL
  ,UNIQUE (label)
  );
CREATE UNIQUE INDEX ON "CallType" (label);

INSERT INTO "CallType" (id, label) VALUES
  (1, 'Информационный звонок'),
  (2, 'Новый кейс'),
  (3, 'Вторичное обращение');
