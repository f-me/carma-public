BEGIN;

INSERT INTO "Role" (id, value, label, isBack) VALUES
(44, 'badmin', 'Бизнес-администратор', 'f');

INSERT INTO "FieldPermission" (role, model, field, r, w) VALUES
  (44, 'FieldPermission', 'role', 't', 't'),
  (44, 'FieldPermission', 'model', 't', 't'),
  (44, 'FieldPermission', 'field', 't', 't'),
  (44, 'FieldPermission', 'r', 't', 't'),
  (44, 'FieldPermission', 'w', 't', 't');

END;
