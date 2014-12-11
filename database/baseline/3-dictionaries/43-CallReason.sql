CREATE TABLE "CallReason"
  (id SERIAL PRIMARY KEY
  ,label text NOT NULL
  ,parent int4  NOT NULL REFERENCES "CallerType" ON DELETE CASCADE
  );

CREATE UNIQUE INDEX ON "CallReason" (parent, label);

INSERT INTO "CallReason" (id, parent, label) VALUES
(1, 1, 'Проверка участия в программе'),
(2, 1, 'Контакт с дилером'),
(3, 1, 'Перевод на горячую линию'),
(4, 1, 'Жалоба'),
(5, 1, 'Другое'),

(6, 2, 'Закрыть заявку'),
(7, 2, 'Соединить с бухгалтерией'),
(8, 2, 'Соединить с менеджером по партнерам'),
(9, 2, 'Жалоба'),
(10, 2, 'Другое'),

(11, 3, 'Соединить с аналитиком'),
(12, 3, 'Соединить с менеджером по программе'),
(13, 3, 'Жалоба'),
(14, 3, 'Другое'),

(15, 4, 'Другое'),

(16, 5, 'Другое');
