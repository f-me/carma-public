BEGIN;
-- fix broken sequence first
SELECT setval('"Dictionary_id_seq"', (SELECT MAX(id) FROM "Dictionary"));
INSERT INTO "Dictionary" (name, description, majorfields)
  VALUES ('Usermeta', 'Пользователи', '{id, login, realName}');

END;
